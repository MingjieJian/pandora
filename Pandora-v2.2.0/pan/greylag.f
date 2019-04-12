      subroutine GREYLAG
     $(X,W,XPBH,XPBHE1,XPBHE2,S,EV,V,VL,G,FINJ,FINJL,XQ,FF,AA,FJIN,
     $ FJINL,Z,TE,E,NO,DUMP,FCK,FCJ)
C
C     Rudolf Loeser, 1984 Jul 21
C---- Computes Hydrogen collisional excitation and ionization rates
C     due to fast electrons.
C
C     NO is the LUN for regular output; DUMP is the general dump switch.
C     !DASH
      save
C     !DASH
      real*8 AA, CON15, CON50, CUTFE, DQMAX, DQMIN, E, EV, FAC, FCJ,
     $       FCK, FF, FINJ, FINJL, FJIN, FJINL, G, HALF, RCCFE, S, TE,
     $       TWO, V, VL, VMNFE, W, X, XINT, XJFE, XPBH, XPBHE1, XPBHE2,
     $       XQ, XQMAX, Z, dummy
      integer I, J, N, NL, NNDFE, NO, NVDFE, NVF, NXF, NZDFE
      logical DMPI, DUMP, KILROY
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(47),NVF)
      equivalence (JZQ(48),NXF)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 84),XJFE )
      equivalence (RZQ( 80),VMNFE)
      equivalence (RZQ( 81),XQMAX)
      equivalence (RZQ( 82),DQMIN)
      equivalence (RZQ( 83),DQMAX)
      equivalence (RZQ( 46),RCCFE)
      equivalence (RZQ( 58),CUTFE)
      equivalence (KZQ( 81),NZDFE)
      equivalence (KZQ( 79),NVDFE)
      equivalence (KZQ( 80),NNDFE)
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external CAVALL, ANKER, FINCH, MINCH, RIGEL, MESHED, MASHED,
     $         POTOO, HI, BYE
C
      dimension X(*), W(*)
C
C               LG = max(NVF,NL)
C
C               V(NVF), EV(N), FCK(N), FCJ(N,NL), FJIN(NL), FINJL(NVF),
      dimension V(*),   EV(*), FCK(*), FCJ(N,*),  FJIN(*),  FINJL(*),
C
C               XPBHE1(Lenpbl), XPBHE2(Lenpbl), FINJ(NVF), TE(N), Z(N),
     $          XPBHE1(*),      XPBHE2(*),      FINJ(*),   TE(*), Z(*),
C
C               VL(NVF), XQ(NXF), S(N), E(N), XPBH(Lenpbl), FJINL(NVF),
     $          VL(*),   XQ(*),   S(*), E(*), XPBH(*),      FJINL(*),
C
C               FF(NXF), AA(NXF), G(NG)
     $          FF(*),   AA(*),   G(*)
C     !EJECT
C
      call HI ('GREYLAG')
C     !BEG
      call RIGEL        (15, CON15)
      call RIGEL        (50, CON50)
      FAC = XJFE*CON50
C
C---- Compute electron energy
      call CAVALL       (X, W, XPBH, XPBHE1, XPBHE2, S, EV, NO)
C
      KILROY = .true.
C---- Compute rates
      do 101 I = 1,N
        call ANKER      (DUMP, N, NZDFE, I, DMPI)
        if(DMPI) then
          if(KILROY) then
            KILROY = .false.
            call MESHED ('GREYLAG', 2)
          end if
        end if
        E(I) = EV(I)/CON15
        if(E(I).gt.HALF) then
          call FINCH    (E(I), NVF, VMNFE, V, VL, G, FINJ, FINJL, XINT,
     $                   XQ, FF, AA, NXF, XQMAX, DQMIN, DQMAX, RCCFE,
     $                   CUTFE, NVDFE, DMPI)
          FCK(I) = FAC*XINT
          call MINCH    (E(I), NL, G, FJIN, FJINL, dummy, XQ, FF, AA,
     $                   NXF, XQMAX, DQMIN, DQMAX, RCCFE,
     $                   CUTFE, NNDFE, DMPI)
          do 100 J = 2,NL
            FCJ(I,J) = FAC*FJIN(J)
  100     continue
        end if
  101 continue
      if(.not.KILROY) then
        call MASHED     ('GREYLAG')
      end if
C
C---- Print
      call POTOO        (NO, N, NL, XJFE, CON15, NVF, VMNFE, Z, TE, S,
     $                   EV, E, FCK, FCJ)
C     !END
      call BYE ('GREYLAG')
C
      return
      end
