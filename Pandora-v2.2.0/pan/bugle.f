      subroutine BUGLE
     $(NO,L,LIM)
C
C     Rudolf Loeser, 1981 Aug 24
C---- Writes a heading, for CHARM.
C     (This is version 2 of BUGLE.)
C     !DASH
      save
C     !DASH
      integer IQMIX, L, LIM, NO
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(217),IQMIX)
C     !DASH
      external ABJECT, NAVAGA, BEAGLE, LINER, HI, BYE
C     !EJECT
C
      call HI ('BUGLE')
C     !BEG
      if(NO.gt.0) then
C
        if(L.eq.1) then
          if(IQMIX.gt.0) then
            call NAVAGA (NO,'RECOMB',6)
            call LINER  (2,NO)
          else
            call ABJECT (NO)
          end if
C
          call BEAGLE   (NO)
        end if
C
        if(LIM.eq.2) then
          call LINER    (4,NO)
          if(L.eq.1) then
            write (NO,100) 'Without'
  100       format(' ',A,' supplementary levels contributions to SQC',
     $                   ' and SQR')
          else
            write (NO,100) 'Including'
          end if
          call LINER    (2,NO)
        end if
C
        write (NO,101)
  101   format(' ',8X,'TE',7X,'TQS',7X,'SQS',7X,'SQC',6X,'SQR',7X,'RRC',
     $             6X,'CRR',6X,'WRR',5X,'X',9X,'H',7X,'DEP',7X,'DRC',
     $             8X,'SA')
        call LINER      (1,NO)
      end if
C     !END
      call BYE ('BUGLE')
C
      return
      end
