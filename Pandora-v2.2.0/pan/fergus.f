      subroutine FERGUS
     $(ABSCI,ORDIN,K,CRIT,BLIM,RLIM)
C
C     Rudolf Loeser, 1991 Jun 20
C---- Looks for abscissa-limits for profile plots.
C     !DASH
      save
C     !DASH
      real*8 ABSCI, BLIM, CRIT, ORDIN, RAT, RLIM, ZERO
      integer I, K
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- MOSTAR      as of 2000 Sep 26
      real*8      COREWL,COREWN
      integer     ICORE
      character   WLAB1*10,WLAB2*2,WLAB3*12,WLAB4*10,WLAB5*2
      logical     WAVENO,SLINE,WHOLE,RED,BLUE
      common      /MOSTAR1/ COREWL,COREWN
      common      /MOSTAR2/ ICORE
      common      /MOSTAR3/ WLAB1,WLAB2,WLAB3,WLAB4,WLAB5
      common      /MOSTAR4/ WAVENO,SLINE,WHOLE,RED,BLUE
C     Wavelength/Wavenumber print/plot controls (subroutine WUMBLE).
C     .
C     !DASH
      external  DIVIDE, HI, BYE
      intrinsic abs
C
C               ABSCI(KM), ORDIN(KM)
      dimension ABSCI(*),  ORDIN(*)
C     !EJECT
C
      call HI ('FERGUS')
C     !BEG
      RLIM = ZERO
      if(WHOLE.or.RED) then
C----   Red limit needed
        do 100 I = K,2,-1
          call DIVIDE ((ORDIN(I-1)-ORDIN(I)),ORDIN(I-1),RAT)
          if(abs(RAT).gt.CRIT) then
            RLIM = ABSCI(I-1)
            go to 101
          end if
  100   continue
  101   continue
      end if
C
      BLIM = ZERO
      if(WHOLE.or.BLUE) then
C----   Blue limit needed
        do 102 I = 2,K,+1
          call DIVIDE ((ORDIN(I)-ORDIN(I-1)),ORDIN(I),RAT)
          if(abs(RAT).gt.CRIT) then
            BLIM = ABSCI(I)
            go to 103
          end if
  102   continue
  103   continue
      end if
C     !END
      call BYE ('FERGUS')
C
      return
      end
