      subroutine OSMOND
     $(NO,NWV,WAVES,YWAVE,KNEG)
C
C     Rudolf Loeser, 1992 Aug 04
C---- Prints "additional" wavelengths for ABAKAN.
C     !DASH
      save
C     !DASH
      real*8 WAVES, YWAVE
      integer I, K, KNEG, NO, NWV
      character BLANK*1, LINE1*63, LINE2*63, SAVE1*5, SAVE2*5
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external LINER, SHIM, BALDWIN, HI, BYE
C
C               WAVES(NWV), YWAVE(NWV)
      dimension WAVES(*),   YWAVE(*)
C     !EJECT
C
      call HI ('OSMOND')
C     !BEG
      if((NO.gt.0).and.(NWV.gt.0)) then
        call             LINER (1,NO)
        write (NO,100)
  100   format(' ','"Additional" Wavelengths, for Continuum ',
     $             'Calculations.'/
     $         ' ','(Wavelengths in Angstroms and some form of meter, ',
     $             'as indicated)')
        call             LINER (1,NO)
C
        K = (NWV+1)/2
        KNEG = 0
        do 102 I = 1,K
          LINE1 = BLANK
          LINE2 = BLANK
C
          call BALDWIN   (WAVES(I  ),YWAVE(I  ),LINE1,I  ,I,SAVE1)
          if((I+K).le.NWV) then
            call BALDWIN (WAVES(I+K),YWAVE(I+K),LINE2,I+K,I,SAVE2)
          end if
C
          write (NO,101) LINE1,LINE2
  101     format(' ',2A63)
          call SHIM      (I,5,NO)
C
          if((WAVES(I).lt.ZERO).or.(WAVES(I+K).lt.ZERO)) then
            KNEG = KNEG+1
          end if
  102   continue
      end if
C     !END
      call BYE ('OSMOND')
C
      return
      end
