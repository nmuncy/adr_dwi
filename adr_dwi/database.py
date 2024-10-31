"""Title.

TODO

"""

import os
import pandas as pd
from contextlib import contextmanager
import pymysql
import paramiko
from sshtunnel import SSHTunnelForwarder
from iterate_rsfmri import helper

type PT = str | os.PathLike
log = helper.MakeLogger(os.path.basename(__file__))


class DbConnect:
    """Supply db_iterate_rsfmri database connection and interaction methods.

    Requires environment variable 'SQL_PASS' to contain user password
    for mysql db_iterate_rsfmri.

    Requires:
        OS gLobal variable SQL_PASS: User password to db_iterate_rsfmri.
        OS global variable RSA_GIMLI: Path to RSA key for SSH access to Gimli.

    Attributes:
        con : mysql.connector.connection_cext.CMySQLConnection
            Connection object to database

    Example:
        db_con = DbConnect()
        row = db_con.fetch_rows("select * from ref_subj limit 1")
        db_con.close_con()

    """

    def __init__(self):
        """Initialize DbConnect and start connection."""
        log.write.info("Initializing database.DbConnect")
        self._check_keys()
        self._connect_ssh()

        log.write.info("Setting attribute 'con' as mysql connection")
        self.con = pymysql.connect(
            host="127.0.0.1",
            user=os.environ["USER"],
            passwd=os.environ["SQL_PASS"],
            db="db_iterate_rsfmri",
            port=self._ssh_tunnel.local_bind_port,
        )

    def _check_keys(self):
        """Title."""
        log.write.info("Checking access keys")
        try:
            os.environ["SQL_PASS"]
        except KeyError as e:
            raise Exception(
                "No global variable 'SQL_PASS' defined in user env"
            ) from e
        try:
            os.environ["RSA_GIMLI"]
        except KeyError as e:
            raise Exception(
                "No global variable 'RSA_GIMLI' defined in user env"
            ) from e

    def _connect_ssh(self):
        """Start ssh tunnel."""
        log.write.info("Starting SSH tunnel")
        rsa_gimli = paramiko.RSAKey.from_private_key_file(
            os.environ["RSA_GIMLI"]
        )
        self._ssh_tunnel = SSHTunnelForwarder(
            ("10.64.118.79", 22),
            ssh_username=os.environ["USER"],
            ssh_pkey=rsa_gimli,
            remote_bind_address=("127.0.0.1", 3306),
        )
        self._ssh_tunnel.start()

    @contextmanager
    def _con_cursor(self):
        """Yield cursor."""
        cursor = self.con.cursor()
        try:
            yield cursor
        finally:
            cursor.close()

    def exec_cmd(self, sql_cmd: str):
        """Execute single command."""
        log.write.info(f"Executing {sql_cmd}")
        with self._con_cursor() as cur:
            cur.execute(sql_cmd)
            self.con.commit()

    def exec_many(self, sql_cmd: str, value_list: list):
        """Update db_iterate_rsfmri via executemany.

        Args:
            sql_cmd: SQL syntax to be executed.
            value_list: List of tuples of data for insertion.

        Example:
            db_con = database.DbConnect()
            sql_cmd = (
                "insert ignore into ref_subj "
                + "(subj_id, subj_name) values (%s, %s)"
            )
            tbl_input = [(1, "00001"), (2, "00002")]
            db_con.exec_many(sql_cmd, tbl_input)

        """
        log.write.info(f"Executing {sql_cmd}")
        with self._con_cursor() as cur:
            cur.executemany(sql_cmd, value_list)
            self.con.commit()

    def fetch_df(self, sql_cmd: str, col_names: list) -> pd.DataFrame:
        """Return dataframe from query output.

        Args:
            sql_cmd: SQL syntax to be executed.
            col_names: Column names to table

        Example:
            db_con = database.DbConnect()
            sql_cmd = "select * from ref_subj"
            col_names = ["subj_id", "subj_name"]
            df_subj = db_con.fetch_df(sql_cmd, col_names)

        """
        log.write.info(f"Building df from {sql_cmd}")
        return pd.DataFrame(self.fetch_rows(sql_cmd), columns=col_names)

    def fetch_rows(self, sql_cmd: str) -> list:
        """Return rows from query output.

        Args:
            sql_cmd: SQL syntax to be executed.

        Example:
            db_con = database.DbConnect()
            sql_cmd = "select * from ref_subj"
            rows = db_con.fetch_df(sql_cmd)

        """
        log.write.info(f"Executing {sql_cmd}")
        with self._con_cursor() as cur:
            cur.execute(sql_cmd)
            rows = cur.fetchall()
        return rows

    def close_con(self):
        """Close database connection."""
        log.write.info("Closing db connection")
        self.con.close()
