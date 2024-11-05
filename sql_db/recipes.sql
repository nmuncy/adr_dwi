-- Test reference
create table ref_test (
    test_id int not null,
    test_name varchar(4),
    primary key(test_id)
);
insert into ref_test
    (test_id, test_name)
    values
    (0, "base"),
    (1, "fu1"),
    (2, "fu2"),
    (3, "fu3"),
    (4, "fu4"),
    (9, "twin");


-- Subject reference
create table ref_subj (
    subj_id int not null,
    subj_name char(4) not null,
    sky_type int not null,
    sky_name char(4) not null,
    primary key(subj_id, sky_type),
    foreign key(sky_type) references ref_test(test_id)
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
    scan_id int not null,
    scan_date date,
    primary key(subj_id, scan_id),
    foreign key(subj_id) references ref_subj(subj_id) on delete cascade,
    foreign key(scan_id) references ref_test(test_id)
);

-- Impact user
create table tbl_impact_user(
    subj_id int not null,
    test_id int not null,
    comp_verbal_memory int,
    comp_visual_memory int,
    comp_visual_motor numeric(4,2),
    comp_react_time numeric(3,2),
    comp_impulse_ctrl int,
    num_hours_slept numeric(2,1),
    symp_total_score int,
    symp_1 int,
    symp_2 int,
    symp_3 int,
    symp_4 int,
    symp_5 int,
    symp_6 int,
    symp_7 int,
    symp_8 int,
    symp_9 int,
    symp_10 int,
    symp_11 int,
    symp_12 int,
    symp_13 int,
    symp_14 int,
    symp_15 int,
    symp_16 int,
    symp_17 int,
    symp_18 int,
    symp_19 int,
    symp_20 int,
    symp_21 int,
    symp_22 int,
    symp_delayed_1 int,
    symp_delayed_2 int,
    symp_delayed_3 int,
    symp_delayed_4 int,
    symp_delayed_5 int,
    symp_delayed_6 int,
    symp_delayed_7 int,
    symp_delayed_8 int,
    symp_delayed_9 int,
    symp_delayed_10 int,
    symp_delayed_11 int,
    symp_delayed_12 int,
    symp_delayed_13 int,
    symp_delayed_14 int,
    symp_delayed_15 int,
    symp_delayed_16 int,
    symp_delayed_17 int,
    symp_delayed_18 int,
    symp_delayed_19 int,
    symp_delayed_20 int,
    symp_delayed_21 int,
    symp_delayed_22 int,
    primary key(subj_id, test_id),
    foreign key(subj_id) references ref_subj(subj_id) on delete cascade,
    foreign key(test_id) references ref_test(test_id)
);

--
create table tbl_test_dates(
    subj_id int not null,
    test_id int not null,
    test_date date,
    primary key(subj_id, test_id),
    foreign key(subj_id) references ref_subj(subj_id) on delete cascade,
    foreign key(test_id) references ref_test(test_id)
);