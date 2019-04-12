      subroutine OSUNA
     $(IMAGE,WAVLOG,DATA,LOG,L,NW,KODA,DATAA,LFB)
C
C     Rudolf Loeser, 1982 May 12
C---- Enters data into a Continuous Spectrum plot.
C     !DASH
      save
C     !DASH
      real*8 DATA, DATAA, EMU, WAVLOG
      integer I, J, KODA, L, LFB, LOG, NW
      logical INCRAD
      character IMAGE*(*), STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external  SARDON, DOWNY, HI, BYE
      intrinsic mod
C
C               DATA(Nmkuse,L), DATAA(Nmkuse), WAVLOG(Nmkuse)
      dimension DATA(NW,*),     DATAA(*),      WAVLOG(*)
C
      data EMU /1.D0/
C
      call HI ('OSUNA')
C     !BEG
      do 100 J = 1,L
        I = mod((J-1),26)+1
        call SARDON   (IMAGE,WAVLOG,DATA(1,J),NW,ALPHS(I),LOG)
  100 continue
C
      if(KODA.eq.1) then
        call DOWNY    (LFB,EMU,INCRAD)
        if(INCRAD) then
          call SARDON (IMAGE,WAVLOG,DATAA    ,NW,STAR    ,LOG)
        end if
      end if
C     !END
      call BYE ('OSUNA')
C
      return
      end
