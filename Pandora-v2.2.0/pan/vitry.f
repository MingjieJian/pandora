      subroutine VITRY
     $(LU,JLEV,J,XLM,RNU,RCP,N,TAUK,XJNU,XLTIT)
C
C     Rudolf Loeser, 1994 Sep 28
C---- Prints rates integration summary for level JLEV.
C     !DASH
      save
C     !DASH
      real*8 ONE, RCP, RNU, TAUK, XJNU, XLM, XLTIT
      integer INDX, J, JLEV, LU, N
      character LEGEND*49, qummy*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  ABJECT, LINER, NEARSD, MONGOL, SHIM, HI, BYE
C
C               TAUK(N), XJNU(N)
      dimension TAUK(*), XJNU(*)
C
      call HI ('VITRY')
C     !BEG
      if(LU.gt.0) then
C
        if(J.eq.1) then
          call ABJECT (LU)
          write (LU,100) JLEV
  100     format(' ','Continuum Integration summary for level ',I3///
     $           ' ',46X,'-------  Near TAUK = 1  ------'/
     $           ' ',6X,'Wavelength (A)',8X,'RRNU',8X,'RRCP',12X,
     $               'TAUK',13X,'JNU')
        end if
C
        call SHIM     ((J-1), 5, LU)
        call NEARSD   (TAUK, N, ONE, INDX)
        call MONGOL   (XLTIT, LEGEND, qummy)
C
        write (LU,101) XLM,RNU,RCP,TAUK(INDX),XJNU(INDX),LEGEND
  101   format(' ',1PE20.12,2E12.5,2E16.8,2X,A49)
      end if
C     !END
      call BYE ('VITRY')
C
      return
      end
