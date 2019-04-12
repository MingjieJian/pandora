      subroutine HIMRE
     $(KZXST,Z,N,NO)
C
C     Rudolf Loeser, 2007 Jan 25
C---- Checks input Z table.
C     !DASH
      save
C     !DASH
      real*8 DELTA, Z
      integer I, IFLAG, JMON, JSM, JZP, KZXST, N, NO
C     !DASH
      external IS_INCREASING, ABORT, COMPD, MESHED, LINER, MASHED,
     $         HI, BYE
C
C               Z(N)
      dimension Z(*)
C
      data DELTA /1.D-6/
C
      call HI ('HIMRE')
C     !BEG
      if(KZXST.gt.0) then
        JZP = 0
        call IS_INCREASING (Z, 1, N, 1, JMON)
        if(JMON.gt.0) then
          call LINER       (2, NO)
          write (NO,100) N
  100     format(' ','Input Z table, of length ',I5)
          call LINER       (1, NO)
          write (NO,101) (Z(I),I=1,N)
  101     format(' ',1P5E18.8)
          call LINER       (1, NO)
          JZP = 1
          write (NO,102) JMON
  102     format(' ','The Z-table is not monotonic at the ',I4,
     $               'th point.')
          call ABORT
        end if
C       !EJECT
        JSM = 0
        do 103 I = 2,N
          call COMPD       (Z(I-1), Z(I), DELTA, IFLAG)
          if(IFLAG.eq.0) then
            JSM = JSM+1
          end if
  103   continue
        if(JSM.gt.0) then
          call MESHED      ('HIMRE', 3)
          if(JZP.le.0) then
            call LINER     (2, NO)
            write (NO,100) N
            call LINER     (1, NO)
            write (NO,101) (Z(I),I=1,N)
            call LINER     (1, NO)
          end if
          write (NO,104) JSM
  104     format(' ','Input values of the Z-grid points are given ',
     $               'to 9 figures.'/
     $           ' ','PANDORA frequently uses delta-Z values, the ',
     $               'differences between successive grid points.'//
     $           ' ','In this run, ',I4,' pairs of successive ',
     $               'Z-values are the same to about 6 figures or ',
     $               'more.')
          call MASHED      ('HIMRE')
        end if
      end if
C     !END
      call BYE ('HIMRE')
C
      return
      end
