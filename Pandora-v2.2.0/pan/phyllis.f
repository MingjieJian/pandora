      subroutine PHYLLIS
     $(NO,TEXP,VXS,N,TVM,TVSB,IQPZT,IZERO)
C
C     Rudolf Loeser, 1983 Sep 30
C---- Prints explanations for "ATMOSPHERE" printout.
C     (This is version 2 of PHYLLIS.)
C     !DASH
      save
C     !DASH
      real*8 VXS
      integer IQPZT, IZERO, K, N, NO
      logical TEXP, TVM, TVSB, TVX, ZVXS
C     !DASH
      external LINER, NAUGHTD, VARAN, HI, BYE
C
C               VXS(N)
      dimension VXS(*)
C
      call HI ('PHYLLIS')
C     !BEG
      if(NO.gt.0) then
        call NAUGHTD (VXS, 1, N, ZVXS)
        TVX = TVSB.or.(.not.ZVXS)
        if(TEXP.or.TVX.or.TVM) then
          call LINER (2, NO)
          write (NO,100)
  100     format(' ','N O T E S')
          call LINER (1, NO)
          call VARAN (NO, IZERO, 'JZATMO')
        end if
C
        if(TEXP) then
          write (NO,101)
  101     format(' ','"Excitation Temperature":'/
     $           ' ',10X,'Excitation Temperature is used in place of ',
     $               'Kinetic Temperature to determine ',
     $               'Electron-Atom collision rates.')
          call LINER (1, NO)
        end if
C     !EJECT
        if(TVX) then
          K = 0
          if(TVSB) then
            K = K+1
          end if
          if(.not.ZVXS) then
            K = K+1
          end if
          if(K.gt.1) then
            write (NO,102)
  102       format(' ','"Expansion" velocities:'/
     $             ' ',10X,'are positive in the outward direction.')
          else
            write (NO,103)
  103       format(' ','"Expansion" velocity:'/
     $             ' ',10X,'is positive in the outward direction.')
          end if
          call LINER (1, NO)
        end if
C
        if(TVM) then
          write (NO,104)
  104     format(' ','"Mass-motion" velocity:'/
     $           ' ',10X,'is positive in the inward direction.')
          call LINER (1, NO)
        end if
C
        if(IQPZT.le.0) then
          write (NO,105)
  105     format(' ','A supplemental printout of Z, TE, NE and NH ',
     $               'with 9 significant figures appears when ',
     $               'option ZPRNT is ON.')
          call LINER (3, NO)
        end if
      end if
C     !END
      call BYE ('PHYLLIS')
C
      return
      end
