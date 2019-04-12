#
# <- Last updated: Thu Oct 31 16:48:42 2013 -> SGK
#
OBJS = buck.o     force_lower.o  is_digit.o  meshow.o   raknts.o  rawout.o \
       calypso.o  force_upper.o  lack.o      mespac.o   raleng.o  run_data.o \
       cnvchr.o   get_date.o     luck.o      meupda.o   raopen.o  scrap.o \
       cnvchrl.o  get_time.o     meacpt.o    month.o    raread.o  second.o \
       cnvchru.o  hostage.o      mefull.o    muck.o     rareri.o  smthday.o \
       dsptime.o  iofault.o      meknts.o    pack.o     rarite.o  duck.o \
       ionname.o  memoir.o       percord.o   rashow.o   timeday.o elapsed.o \
       is_bad.o   mertrn.o       puck.o      rawinn.o   tuck.o is_xxx.o
#
syslib.a: $(OBJS)
	ar r $@ $(OBJS)
#
clean:
	-rm *.o
cleaner:
	-rm *.o *.a
