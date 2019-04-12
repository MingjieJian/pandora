      subroutine PLEASE
     $(T5,F,NTITER,HND,G,H,Z,RAT,HNDO,HNDT,V,C,EM,FEM,OP5000,TAU5000)
C
C     Rudolf Loeser, 1980 Jul 22
C---- Computes a new trial table of HND, for the H.S.E. calculation.
C     (This is version 3 of PLEASE.)
C     !DASH
      save
C     !DASH
      real*8 C, EM, F, FEM, G, H, HND, HNDO, HNDT, ONE, OP5000, RAT, T5,
     $       T5A, TAU5000, V, Z
      integer LHHSE, N, NTITER
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (KZQ(129),LHHSE)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external MOVE1, WROTH, FLIVVER, ELAPSE, DIVIDE, WENDY, HYPATIA,
     $         HAND, HI, BYE
C
C               HNDT(N), HNDO(N), TAU5000(N), F(N), HND(N), H(N), Z(N),
      dimension HNDT(*), HNDO(*), TAU5000(*), F(*), HND(*), H(*), Z(*),
C
C               G(N), T5(12), OP5000(N), EM(N)
     $          G(*), T5(*),  OP5000(*), EM(*)
C
      call HI ('PLEASE')
C     !BEG
C---- Save current HND
      call MOVE1     (HND, N, HNDO)
C---- Compute current adjustment factor, whose purpose is to produce
C     a set of HND values that will lead to Tau5000(ref)=1.
      call ELAPSE    (NTITER, T5, T5A)
      call DIVIDE    (HNDO(LHHSE), (G(LHHSE)*T5A), RAT)
C---- Check to see RAT is all right (using HNAJL)
      call HYPATIA   (RAT, Z, HNDO, OP5000, TAU5000, N)
C---- Compute new trial HNDX
      call HAND      (RAT, N, G, H, ONE, HNDT)
      if(LHHSE.eq.1) then
C----   Compute V and C
        call FLIVVER (N, HNDO, HNDT, G, H, Z, HND, F(NTITER), V, C)
      else
        V = ONE
        C = ONE
      end if
C---- Set up final HNDX
      if(V.eq.ONE) then
        call MOVE1   (HNDT, N, HND)
      else
        call HAND    (RAT, N, G, H, C, HND)
      end if
C---- Compute and check final HND, which includes magnetic term
      call WROTH     (N, EM, FEM, HND)
C---- Continuum Recalculation control
      call WENDY     (HND, 1, N, 4, 'PLEASE')
C     !END
      call BYE ('PLEASE')
C
      return
      end
