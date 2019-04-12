      subroutine RAH
     $(NO,NW,WAVE,A,IND,N,YAYB)
C
C     Rudolf Loeser, 15 Jun 71
C---- prints YAYB, for OSIRIS.
C     !DASH
      save
C     !DASH
      real*8 A, WAVE, YAYB
      integer I, IE, IND, IS, J, N, NO, NW
      character BLANK*1, MARK*1, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external  MARKI, LINER, SHIM, HI, BYE
      intrinsic min
C
C               WAVE(NW), A(NW), IND(NW), YAYB(N,NW)
      dimension WAVE(*),  A(*),  IND(*),  YAYB(N,*)
C
      call HI ('RAH')
C     !BEG
      if(NO.gt.0) then
        IE = 0
  100   continue
          IS = IE+1
          IE = min(IE+10,N)
C
          call LINER   (2, NO)
          write (NO,101) (I,I=IS,IE)
  101     format(' ',21X,'Mean Intensities'//
     $           ' ',3X,'LHM',7X,'AHM',3X,10I10)
          call LINER   (1, NO)
C
          do 103 J = 1,NW
            call MARKI (IND(J), 1, MARK, STAR, BLANK)
            write (NO,102) WAVE(J),MARK,A(J),(YAYB(I,J),I=IS,IE)
  102       format(' ',F6.0,A1,F9.5,3X,1P10E10.2)
            call SHIM  (J, 5, NO)
  103     continue
C
        if(IE.lt.N) goto 100
      end if
C     !END
      call BYE ('RAH')
C
      return
      end
