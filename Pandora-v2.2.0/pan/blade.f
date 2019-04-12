      subroutine BLADE
     $(I,L,N,NW,TAU,EMU,TMU,EMINT,SNU,WS,MUX,YY,KODE,CNXP,EMINTA,WTAB,
     $ LFB,Z,DIDH,MYX,SU,LININT,DODIDH,W)
C
C     Rudolf Loeser, 1980 Jun 13
C---- Sets up continuum intensity data, for all look-angles at the I'th
C     wavelength, for VISHNU.
C     (This is version 2 of BLADE.)
C     !DASH
      save
C     !DASH
      real*8 CNXP, DIDH, EMINT, EMINTA, EMU, SNU, SU, TAU, TMU, W, WS,
     $       WTAB, YY, Z
      integer I, J, KODE, L, LFB, MUX, MYX, N, NW
      logical DODIDH, INCRAD, LININT
C     !DASH
      external TANG, SIMBA, DOWNY, PINK, MOVE1, DANZIG, HI, BYE
C
      dimension W(*)
C
C               EMINT(Nmkuse,L), KODE(Nmkuse,L), MYX(Nmkuse,L), EMU(L),
      dimension EMINT(NW,*),     KODE(NW,*),     MYX(NW,*),     EMU(*),
C
C               TMU(N), WS(N,L), CNXP(N), MUX(Nmkuse,L), TAU(N), SU(N),
     $          TMU(*), WS(N,*), CNXP(*), MUX(NW,*),     TAU(*), SU(*),
C
C               EMINTA(Nmkuse), DIDH(N,Nmkuse,L), YY(Nmkuse,L), SNU(N),
     $          EMINTA(*),      DIDH(N,NW,*),     YY(NW,*),     SNU(*),
C
C               WTAB(Nmkuse), Z(N)
     $          WTAB(*),      Z(*)
C
      call HI ('BLADE')
C     !BEG
      do 100 J = 1,L
        call TANG   (EMU(J), 1, TAU, N, TMU)
        call MOVE1  (SNU, N, SU)
        call SIMBA  (LFB, WTAB(I), N, TMU, SU, WS(1,J), EMINT(I,J),
     $               MUX(I,J), YY(I,J), KODE(I,J))
C
        call DANZIG (DODIDH, N, Z, WS(1,J), SU, DIDH(1,I,J), MYX(I,J),
     $               W)
C
        call DOWNY  (LFB, EMU(J), INCRAD)
        if(INCRAD) then
C----     (Remember: mu=1, so that TMU(N)=TAU(N) !)
          EMINTA(I) = EMINT(I,J)+CNXP(N)
        end if
C
        call PINK   (WTAB(I), EMU(J), EMINT(I,J), MUX(I,J), MYX(I,J),
     $               YY(I,J), KODE(I,J), TAU, TMU, SU, WS(1,J), N, LFB,
     $               LININT)
  100 continue
C     !END
      call BYE ('BLADE')
C
      return
      end
