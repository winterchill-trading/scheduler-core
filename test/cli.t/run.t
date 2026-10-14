Monthly on the 31st clamps to the end of shorter months and does not drift:

  $ scheduler-dates -frequency monthly -start 2027-01-31 -count 4
  0	2027-01-31	SUN
  1	2027-02-28	SUN
  2	2027-03-31	WED
  3	2027-04-30	FRI

Every 10 days, bounded by an end date, and what is owed after a gap:

  $ scheduler-dates -frequency every_n_days -interval 10 -start 2027-03-01 -end 2027-03-31 -due-on 2027-03-25
  0	2027-03-01	MON
  1	2027-03-11	THU
  2	2027-03-21	SUN
  3	2027-03-31	WED
  ((run (((index 2) (date 2027-03-21))))
   (missed (((index 0) (date 2027-03-01)) ((index 1) (date 2027-03-11)))))

Invalid input:

  $ scheduler-dates -frequency weekly -start 2027-03-10 -end 2027-03-01
  End_before_start
  [2]
