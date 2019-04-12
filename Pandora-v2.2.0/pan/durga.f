      subroutine DURGA
     $(NW,L,WAVES,EMINT,BRIGHT,YSTAR,EMINTA,BRIGHTA,YSTARA,LFB)
C
C     Rudolf Loeser, 1971 Jan 12
C---- Computes brightness temperature, and intensity per unit
C     wavelength, from the emergent intensity (/Hz).
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, BRIGHTA, EMINT, EMINTA, ONE, WAVE, WAVES, YSTAR,
     $       YSTARA
      integer I, J, L, LFB, NW
      logical INCRAD
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  DOWNY, OCEAN, PLAY, HI, BYE
      intrinsic abs
C
C               EMINT(Nmkuse,L), BRIGHT(Nmkuse,L), YSTAR(Nmkuse,L),
      dimension EMINT(NW,*),     BRIGHT(NW,*),     YSTAR(NW,*),
C
C               EMINTA(Nmkuse), BRIGHTA(Nmkuse), YSTARA(Nmkuse),
     $          EMINTA(*),      BRIGHTA(*),      YSTARA(*),
C
C               WAVES(Nmkuse)
     $          WAVES(*)
C
      call HI ('DURGA')
C     !BEG
      call DOWNY     (LFB,ONE,INCRAD)
      do 101 I = 1,NW
        WAVE = abs(WAVES(I))
        do 100 J = 1,L
          call OCEAN (WAVE,EMINT(I,J),BRIGHT(I,J))
          call PLAY  (WAVE,EMINT(I,J),YSTAR(I,J))
  100   continue
        if(INCRAD) then
          call OCEAN (WAVE,EMINTA(I),BRIGHTA(I))
          call PLAY  (WAVE,EMINTA(I),YSTARA(I))
        end if
  101 continue
C     !END
      call BYE ('DURGA')
C
      return
      end
