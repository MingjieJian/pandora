      subroutine PIPE
     $(CGR,YH,RFMAS,R1N,KPRSW,CPRSS,CLOGG,LGGIN,PZERO,KTKIN,LINE,NO)
C
C     Rudolf Loeser, 1974 Dec 14
C---- Prints miscellaneous atmosphere data.
C     !DASH
      save
C     !DASH
      real*8 CGR, CLOGG, CPRSS, PZERO, R1N, RFMAS, YH, ZERO
      integer KPRSW, KTKIN, LGGIN, NO
      character LINE*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external LINER, FROGG, HI, BYE
C
      call HI ('PIPE')
C     !BEG
      call LINER   (2, NO)
      if(R1N.gt.ZERO) then
        call LINER (1, NO)
        call FROGG ('Inner radius (km)'       , R1N  , 1, LINE, 1, NO)
      end if
      call LINER   (1, NO)
      call FROGG   ('Additive mass constant'  , RFMAS, 1, LINE, 1, NO)
      if(KTKIN.gt.0) then
        call FROGG ('Surface pressure'        , PZERO, 1, LINE, 1, NO)
      end if
      call LINER   (1, NO)
      if(LGGIN.gt.0) then
        call FROGG ('Log g'                   , CLOGG, 1, LINE, 1, NO)
        call FROGG ('Gravity ratio'           , CGR  , 1, LINE, 1, NO)
      else
        call FROGG ('Gravity ratio'           , CGR  , 1, LINE, 1, NO)
        call FROGG ('Log g'                   , CLOGG, 1, LINE, 1, NO)
      end if
      if(KPRSW.eq.1) then
        call LINER (1, NO)
        call FROGG ('Constant pressure'       , CPRSS, 1, LINE, 1, NO)
      end if
      call LINER   (1, NO)
      call FROGG   ('Helium to Hydrogen ratio', YH   , 1, LINE, 1, NO)
      call LINER   (1, NO)
C     !END
      call BYE ('PIPE')
C
      return
      end
