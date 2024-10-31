-- Subject reference
create table ref_subj (
    subj_id int not null,
    subj_name char(4) not null,
    subj_sky_base int not null,
    subj_sky_fu1 char(4),
    subj_sky_fu2 char(4),
    subj_sky_twin char(4),
    primary key(subj_id)
);

-- Basic demographics
create table tbl_demo (
    subj_id int not null,
    sex enum("M", "F"),
    age_base int,
    primary key(subj_id),
    foreign key(subj_id) references ref_subj(subj_id) on delete cascade
);

-- Scan dates
create table tbl_scan_dates(
    subj_id int not null,
    scan_name enum("base", "fu1", "fu2"),
    scan_date date,
    primary key(subj_id),
    foreign key(subj_id) references ref_subj(subj_id) on delete cascade
);