      subroutine PETIT
     $(N,Z,TE,TES,TER,IQUET,NO)
C
C     Rudolf Loeser, 2001 Nov 29
C---- Prints, for HYDRU.
C     !DASH
      save
C     !DASH
      real*8 TE, TER, TES, Z
      integer I, IQUET, N, NO
      character BLANK*1, MIS*1, MSR*1, STAR*1
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
      external ABJECT, LINER, SHIM, HI, BYE
C
C               Z(N), TE(N), TES(N), TER(N)
      dimension Z(*), TE(*), TES(*), TER(*)
C     !EJECT
C
      call HI ('PETIT')
C     !BEG
      call ABJECT (NO)
      write (NO,100)
  100 format(' ','Massaging the input values of TE . . .'//
     $       ' ',24X,'Z',12X,'input TE', 9X,'smoothed TE',
     $           5X,'rounded TE')
      call LINER  (1,NO)
C
      do 102 I = 1,N
        MIS = BLANK
        if(TES(I).ne.TE(I)) then
          MIS = STAR
        end if
        MSR = BLANK
        if(TER(I).ne.TES(I)) then
          MSR = STAR
        end if
        write (NO,101) I,Z(I),TE(I),TES(I),MIS,TER(I),MSR
  101   format(' ',I5,1P3E20.10,A1,0PF15.0,A1)
        call SHIM (I,5,NO)
  102 continue
C
      call LINER  (1,NO)
      if(IQUET.gt.0) then
        write (NO,103)
  103   format(' ','The final, rounded TE values will be used in ',
     $             'this run, instead of the input.',10X,
     $             '(Option INTEDIT)')
      else
        write (NO,104)
  104   format(' ','The unchanged input TE values will be used in ',
     $             'this run.',10X,'(Option INTEDIT)')
      end if
C     !END
      call BYE ('PETIT')
C
      return
      end
