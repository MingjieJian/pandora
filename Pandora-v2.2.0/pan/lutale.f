      subroutine LUTALE
     $(ABSCI,ORDIN,K,PROGLI,XL,XU)
C
C     Rudolf Loeser, 1991 Feb 20
C---- Sets up abscissa limits for profile graphs.
C     !DASH
      save
C     !DASH
      real*8 ABSCI, BLIM, BX, CRIT, ORDIN, PROGLI, RLIM, RX, XL, XU,
     $       ZERO
      integer K
      logical FNLM, USLM
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
      external  FERGUS, HI, BYE
      intrinsic abs
C
C               ABSCI(KM), ORDIN(KM)
      dimension ABSCI(*),  ORDIN(*)
C
      call HI ('LUTALE')
C     !BEG
      if(WAVENO) then
        if(PROGLI.gt.ZERO) then
          BX = COREWN+PROGLI
          RX = COREWN-PROGLI
        end if
      end if
C
      USLM = PROGLI.gt.ZERO
      FNLM = PROGLI.lt.ZERO
      if(FNLM) then
        CRIT = abs(PROGLI)
        call FERGUS   (ABSCI, ORDIN, K, CRIT, BLIM, RLIM)
      end if
C     !EJECT
      if(ABSCI(1).eq.ZERO) then
C----   Half profile
        if(ABSCI(K).gt.ZERO) then
C----     Red half
          if(WAVENO) then
            XL = ABSCI(K)
          else
            XL = ABSCI(1)
          end if
          if(USLM) then
            if(WAVENO) then
              XU = RX
            else
              XU = +PROGLI
            end if
          else if(FNLM) then
            XU = RLIM
          else
            if(WAVENO) then
              XU = ABSCI(1)
            else
              XU = ABSCI(K)
            end if
          end if
        else
C----     Blue half
          if(WAVENO) then
            XU = ABSCI(K)
          else
            XU = ABSCI(1)
          end if
          if(USLM) then
            if(WAVENO) then
              XL = BX
            else
              XL = -PROGLI
            end if
          else if(FNLM) then
            XL = BLIM
          else
            if(WAVENO) then
              XL = ABSCI(1)
            else
              XL = ABSCI(K)
            end if
          end if
        end if
      else
C     !EJECT
C----   Whole profile
        if(USLM) then
          if(WAVENO) then
            XL = BX
            XU = RX
          else
            XL = -PROGLI
            XU = +PROGLI
          end if
        else if(FNLM) then
          XL = BLIM
          XU = RLIM
        else
          if(WAVENO) then
            XL = ABSCI(K)
            XU = ABSCI(1)
          else
            XL = ABSCI(1)
            XU = ABSCI(K)
          end if
        end if
      end if
C     !END
      call BYE ('LUTALE')
C
      return
      end
