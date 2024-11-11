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


-- Scan reference
create table ref_scan (
    scan_id int not null,
    scan_name varchar(4),
    primary key(scan_id)
);
insert into ref_scan
    (scan_id, scan_name)
    values
    (0, "base"),
    (1, "post"),
    (2, "rtp");


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
    foreign key(scan_id) references ref_scan(scan_id)
);

-- Impact dates
create table tbl_impact_dates(
    subj_id int not null,
    test_id int not null,
    num_tbi int not null,
    impact_date date,
    primary key(subj_id, test_id, num_tbi),
    foreign key(subj_id) references ref_subj(subj_id) on delete cascade,
    foreign key(test_id) references ref_test(test_id)
);

-- Impact user
create table tbl_impact_user(
    subj_id int not null,
    test_id int not null,
    num_tbi int not null,
    userMemoryCompositeScoreVerbal int,
    userMemoryCompositeScoreVisual int,
    userVisualMotorCompositeScore numeric(4,2),
    userReactionTimeCompositeScore numeric(3,2),
    userImpulseControlCompositeScore int,
    userHoursOfSleepLastNight numeric(2,1),
    userTotalSymptomScore int,
    userSymptom1 int,
    userSymptom2 int,
    userSymptom3 int,
    userSymptom4 int,
    userSymptom5 int,
    userSymptom6 int,
    userSymptom7 int,
    userSymptom8 int,
    userSymptom9 int,
    userSymptom10 int,
    userSymptom11 int,
    userSymptom12 int,
    userSymptom13 int,
    userSymptom14 int,
    userSymptom15 int,
    userSymptom16 int,
    userSymptom17 int,
    userSymptom18 int,
    userSymptom19 int,
    userSymptom20 int,
    userSymptom21 int,
    userSymptom22 int,
    userSymptom1Delayed int,
    userSymptom2Delayed int,
    userSymptom3Delayed int,
    userSymptom4Delayed int,
    userSymptom5Delayed int,
    userSymptom6Delayed int,
    userSymptom7Delayed int,
    userSymptom8Delayed int,
    userSymptom9Delayed int,
    userSymptom10Delayed int,
    userSymptom11Delayed int,
    userSymptom12Delayed int,
    userSymptom13Delayed int,
    userSymptom14Delayed int,
    userSymptom15Delayed int,
    userSymptom16Delayed int,
    userSymptom17Delayed int,
    userSymptom18Delayed int,
    userSymptom19Delayed int,
    userSymptom20Delayed int,
    userSymptom21Delayed int,
    userSymptom22Delayed int,
    primary key(subj_id, test_id, num_tbi),
    foreign key(subj_id) references ref_subj(subj_id) on delete cascade,
    foreign key(test_id) references ref_test(test_id)
);

-- Impact word
create table tbl_impact_word(
    subj_id int not null,
    test_id int not null,
    num_tbi int not null,
    wordMemoryHits int,
    wordMemoryHitsDelay int,
    wordMemoryCD int,
    wordMemoryCDDelay int,
    wordMemoryLP numeric(3,2),
    wordMemoryDMCorrect numeric(3,2),
    wordMemoryTotalPercentCorrect numeric(3,2),
    primary key(subj_id, test_id, num_tbi),
    foreign key(subj_id) references ref_subj(subj_id) on delete cascade,
    foreign key(test_id) references ref_test(test_id)
);

-- Impact design
create table tbl_impact_design(
    subj_id int not null,
    test_id int not null,
    num_tbi int not null,
    designMemoryHits int,
    designMemoryHitsDelay int,
    designMemoryCD int,
    designMemoryCDDelay int,
    designMemoryLP numeric(3,2),
    designMemoryDMCorrect numeric(3,2),
    designMemoryTotalPercentCorrect numeric(3,2),
    primary key(subj_id, test_id, num_tbi),
    foreign key(subj_id) references ref_subj(subj_id) on delete cascade,
    foreign key(test_id) references ref_test(test_id)
);

-- Impact XO
create table tbl_impact_xo(
    subj_id int not null,
    test_id int not null,
    num_tbi int not null,
    XOtotalCorrectMemory int,
    XOtotalCorrectInterference int,
    XOtotalIncorrect int,
    XOaverageCorrect numeric(3,2),
    XOaverageIncorrect numeric(3,2),
    primary key(subj_id, test_id, num_tbi),
    foreign key(subj_id) references ref_subj(subj_id) on delete cascade,
    foreign key(test_id) references ref_test(test_id)
);

-- Impact color
create table tbl_impact_color(
    subj_id int not null,
    test_id int not null,
    num_tbi int not null,
    colorMatchTotalCorrect int,
    colorMatchTotalCommissions int,
    colorMatchAverageCorrect numeric(3,2),
    colorMatchAverageCommissions numeric(3,2),
    primary key(subj_id, test_id, num_tbi),
    foreign key(subj_id) references ref_subj(subj_id) on delete cascade,
    foreign key(test_id) references ref_test(test_id)
);

-- Impact three
create table tbl_impact_three(
    subj_id int not null,
    test_id int not null,
    num_tbi int not null,
    threeLettersTotalSequenceCorrect int,
    threeLettersTotalLettersCorrect int,
    threeLettersPercentageLettersCorrect numeric(3,2),
    threeLettersAverageTimeFirstClick numeric(3,2),
    threeLettersAverageCounted numeric(3,1),
    threeLettersAverageCountedCorrectly numeric(3,1),
    primary key(subj_id, test_id, num_tbi),
    foreign key(subj_id) references ref_subj(subj_id) on delete cascade,
    foreign key(test_id) references ref_test(test_id)
);

-- Impact test dates
create table tbl_test_dates(
    subj_id int not null,
    test_id int not null,
    test_date date,
    primary key(subj_id, test_id),
    foreign key(subj_id) references ref_subj(subj_id) on delete cascade,
    foreign key(test_id) references ref_test(test_id)
);