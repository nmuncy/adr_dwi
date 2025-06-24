"""Methods for connecting to, and interacting with, the MySQL database.

A MySQL server runs on a local linux machine (Gimli), hosting
a database called db_adr. These methods allow the user to
connect to, pull data from, and send data to the database.

Notes:
    Currently expects workstation to be named 'gimli' and the compute
    cluster 'swan'. Also accepts Mac users ('Darwin').

build_afq: Send datat to db_adr.tbl_afq.
build_impact_user: Send data to db_adr.tbl_impact_user.
build_table: Send data to db_adr given input.
DbConnect: Connect to MySQL server over SSH connection (or locally)
    and provide methods for interacting with db_adr.
RefMaps: Supply mapping attributes of reference name to ID, and
    other methods, attributes for such mappings.

"""

import os
import platform
import pandas as pd
import numpy as np
from contextlib import contextmanager
from typing import Type
from adr_dwi import helper

if "gimli" in platform.uname().node:
    import mysql.connector
elif "swan" in platform.uname().node or "Darwin" in platform.uname().system:
    import pymysql
    import paramiko
    from sshtunnel import SSHTunnelForwarder

type PT = str | os.PathLike
log = helper.MakeLogger(os.path.basename(__file__))


# %%
class DbConnect:
    """Supply db_adr database connection and interaction methods.

    Requires:
        - OS gLobal variable SQL_PASS: User password to db_adr.
        - OS global variable RSA_WS: Path to RSA key for SSH access to work
            station running MySQL server.

    Attributes:
        con : mysql.connector.connection_cext.CMySQLConnection
            Connection object to database

    Notes:
        Currently depends on machine named 'gimli' for local connection, at
        specified IP address. See initializer and _connect_ssh methods.

    Example:
        db_con = DbConnect()
        row = db_con.fetch_rows("select * from ref_subj limit 1")
        db_con.close_con()

    """

    def __init__(self):
        """Initialize DbConnect."""
        log.write.info("Initializing DbConnect")
        try:
            os.environ["SQL_PASS"]
        except KeyError as e:
            raise Exception(
                "No global variable 'SQL_PASS' defined in user env"
            ) from e

        self._db_name = "db_adr"
        if "gimli" in platform.uname().node:
            self._connect_local()
        elif (
            "swan" in platform.uname().node
            or "Darwin" in platform.uname().system
        ):
            self._connect_remote()

    def _connect_local(self):
        """Connect to MySQL server from work station."""
        log.write.info("Connecting from work station")
        self.con = mysql.connector.connect(
            host="localhost",
            user=os.environ["USER"],
            password=os.environ["SQL_PASS"],
            database=self._db_name,
        )

    def _connect_remote(self):
        """Connect to MySQL server from remote."""
        log.write.info("Connecting from HCC")
        try:
            os.environ["RSA_WS"]
        except KeyError as e:
            raise Exception(
                "No global variable 'RSA_WS' defined in user env"
            ) from e

        self._connect_ssh()
        self.con = pymysql.connect(
            host="127.0.0.1",
            user=os.environ["USER"],
            passwd=os.environ["SQL_PASS"],
            db=self._db_name,
            port=self._ssh_tunnel.local_bind_port,
        )

    def _connect_ssh(self):
        """Start ssh tunnel."""
        log.write.info("Starting SSH tunnel")
        rsa_ws = paramiko.RSAKey.from_private_key_file(os.environ["RSA_WS"])
        self._ssh_tunnel = SSHTunnelForwarder(
            ("gimli", 22),
            ssh_username=os.environ["USER"],
            ssh_pkey=rsa_ws,
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
        """Add data to db_adr via executemany.

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


class RefMaps:
    """Supply mappings to SQL reference table values.

    Query MySQL database to get task, session, subject, and step name
    mappings. Also supplies methods for converting pd.DataFrame cells
    from field name to field ID.

    Args:
        db_con: Instance of database.DbConnect.

    Attribtues:
        col_pipe_prog (list): Column names of tbl_pipe_prog.
        ref_task (dict): Map task name to ID.
        ref_sess (dict): Map sess name to ID.
        ref_subj (dict): Map subj name to ID.
        ref_step (dict): Map step name to ID.

    """

    def __init__(self, db_con: Type[DbConnect]):
        """Initialize _RefMaps."""
        log.write.info("Initializing database.RefMaps")
        self._db_con = db_con
        self._load_cols()
        self._load_ref_subj()
        self._load_refs()
        # self._load_enums()

    def _load_ref_subj(self):
        """Title."""
        self._df_subj = self._db_con.fetch_df(
            " select * from ref_subj",
            self.col_ref_subj,
        )

    def _load_refs(self):
        """Supply reference name-to-ID mappings in format {name: id}."""
        log.write.info("Loading references")

        # Set attributes ref_task, ref_sess, ref_subj, and ref_step.
        for attr_name in ["test", "tract"]:
            df = self._db_con.fetch_df(
                f"select * from ref_{attr_name}",
                [f"{attr_name}_id", f"{attr_name}_name"],
            )
            ref_map = {
                y: x
                for x, y in zip(df[f"{attr_name}_id"], df[f"{attr_name}_name"])
            }
            setattr(self, f"ref_{attr_name}", ref_map)

    def _load_cols(self):
        """Supply column names of select tables in list format."""
        log.write.info("Loading columns")

        #
        self.col_ref_subj = [
            x[0]
            for x in self._db_con.fetch_rows(
                " select column_name from information_schema.columns where "
                + "table_schema = 'db_adr' and table_name = 'ref_subj' "
                + "order by ordinal_position"
            )
        ]

        self.col_impact_user = [
            x[0]
            for x in self._db_con.fetch_rows(
                " select column_name from information_schema.columns where "
                + "table_schema = 'db_adr' and table_name = 'tbl_impact_user' "
                + "order by ordinal_position"
            )
        ]

    def get_id(self, name: str, row: pd.Series, col_name: str) -> int | str:
        """Return attribute ID given name from ref attributes.

        Returns the integer reference IDs given a reference name,
        used to access the class ref attributes and update a column
        of reference names with IDs.

        Args:
            name: Attribute name TODO
            row: Lambda output for axis=1.
            col_name: Key, name of column.

        Returns:
            Reference ID.

        Raises:
            ValueError: Unexpected name value

        """
        if name not in ["test", "tract"]:
            log.write.error(f"Unexpected name: {name}")
            raise ValueError
        ref_attr = getattr(self, f"ref_{name}")

        # TODO refactor
        for ref_name, ref_id in ref_attr.items():
            if row[col_name] == ref_name:
                return ref_id


def build_table(
    tbl_name: str, df: pd.DataFrame, col_list: list, db_con: Type[DbConnect]
):
    """Insert data from pd.DataFrame into table.

    Assumes data in pd.DataFrame is organized for insertion into table.

    Args:
        tbl_name: Destination db_adr table.
        df: Dataframe with appropriate column names, types.
        col_list: Columns of df to insert.
        db_con: Instance of database.DbConnect().

    """
    log.write.info(f"Updating db_adr.{tbl_name}")
    value_list = ["%s" for x in col_list]
    sql_cmd = (
        f"insert ignore into {tbl_name} ({', '.join(col_list)}) "
        + f"values ({', '.join(value_list)})"
    )
    tbl_input = list(df[col_list].itertuples(index=False, name=None))
    db_con.exec_many(sql_cmd, tbl_input)


def build_impact_user(df_user: pd.DataFrame, ref_maps: Type[RefMaps]):
    """Insert data into db_adr.tbl_ipact_user.

    Assumes data in pd.DataFrame is organized for insertion into table.

    Args:
        df_user: Dataframe with appropriate column names, types.
        ref_maps: Instance of database.RefMaps().

    """
    log.write.info("Updating db_adr.tbl_impact_user")
    df_user = df_user.replace(np.nan, None)
    value_list = ["%s" for x in ref_maps.col_impact_user]
    sql_cmd = (
        "insert ignore into tbl_impact_user "
        + f"({', '.join(ref_maps.col_impact_user)}) "
        + f"values ({', '.join(value_list)})"
    )
    tbl_input = list(
        df_user[ref_maps.col_impact_user].itertuples(index=False, name=None)
    )
    ref_maps._db_con.exec_many(sql_cmd, tbl_input)


def build_afq(
    df: pd.DataFrame, rerun: bool = False, rescan: bool = False
) -> pd.DataFrame:
    """Insert data into db_adr.tbl_afq.

    Format pyAFQ output for insertion into database.

    Args:
        df: Dataframe of afq derivative tract_profiles.csv.
        rerun: Optional, get and send rerun metrics instead of all.
        rescan: Optional, get and send scan-rescan metrics instead of all.

    Returns:
        Formatted dataframe used for insertion.

    """
    tbl_name = "tbl_afq"
    if rerun:
        tbl_name = "tbl_afq_rerun"
    if rescan:
        tbl_name = "tbl_afq_rescan"
    log.write.info(f"Sending AFQ data to db_adr.{tbl_name}")

    # Start connection and load references
    db_con = DbConnect()
    ref_maps = RefMaps(db_con)

    # Rename cols and update session value
    col_switch = {
        "tractID": "tract_id",
        "nodeID": "node_id",
        "subjectID": "subj_id",
        "sessionID": "sess_id",
    }
    df = df.rename(columns=col_switch)
    df["sess_id"] = df["sess_id"] - 1

    # Convert tract names to IDs
    df["tract_id"] = df.apply(
        lambda x: ref_maps.get_id("tract", x, "tract_id"), axis=1
    )

    # Build and execute insert command
    df = df.replace(np.nan, None)
    col_list = list(df.columns)
    value_list = ["%s" for x in col_list]
    sql_cmd = (
        f"insert ignore into {tbl_name}"
        + f"({', '.join(col_list)}) "
        + f"values ({', '.join(value_list)})"
    )
    tbl_input = list(df[col_list].itertuples(index=False, name=None))
    db_con.exec_many(sql_cmd, tbl_input)

    # Clean up
    db_con.close_con()
    return df
