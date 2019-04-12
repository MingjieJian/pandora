      subroutine KALMIA
     $(N,CV,Z,HND,RHEAB,FR,FMV,KILROY,TIT,LU,V)
C
C     Rudolf Loeser, 1988 Aug 10
C---- Generates a table of mass-conserving fluid velocities.
C     The constant CV specifies the velocity at the level
C     where NH = 10**10.
C     Assume that HND(i) .ge. HND(i-1), 2 .le. i .le. N.
C     The velocity table will be printed if LU > 0.
C     !DASH
      save
C     !DASH
      real*8 ASTAR, CDZ, CON, CV, CVZ, DIV, FAC, FMV, FR, HEABD, HEMASS,
     $       HND, ONE, RHEAB, TRM, V, Z, ZERO, dummy
      integer I, KODE, LU, MFMV, N
      logical KILROY
      character TIT*(*)
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ(137),CVZ  )
      equivalence (RZQ(138),CDZ  )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(54),MFMV )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 5),HEMASS)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external ALKIMA, DIVIDE, LINER, PRIVET, URANIA, ARRMUL, FRANK,
     $         HI, BYE
C
C               Z(N), HND(N), FR(N), V(N), RHEAB(N), FMV(N)
      dimension Z(*), HND(*), FR(*), V(*), RHEAB(*), FMV(*)
C
      call HI ('KALMIA')
C     !BEG
      if(CV.ne.ZERO) then
        if(KILROY) then
C----     Get common auxiliaries
          KILROY = .false.
          call FRANK    ('HE ',0,HEABD,dummy,dummy,dummy,KODE)
          call URANIA   (N,HND,RHEAB,HEABD,ASTAR)
          call ALKIMA   (N,Z,HND, FAC,FR)
        end if
C----   Compute velocity
        CON = FAC*CV
        do 100 I = 1,N
          TRM = (ONE+HEMASS*ASTAR)/(ONE+HEMASS*HEABD*RHEAB(I))
          DIV = HND(I)*(FR(I)**2)
          call DIVIDE   ((CON*TRM),DIV,V(I))
  100   continue
        if(MFMV.gt.0) then
          call ARRMUL   (V,FMV,V,N)
        end if
C
        if(LU.gt.0) then
C----     Print
          call LINER    (2,LU)
          write (LU,101) TIT,CV,ASTAR,CVZ,CDZ,MFMV
  101     format(' ','Computed ',A,'(NH)',10X,'CV =',1PE12.4,5X,
     $               'a* =',E12.4,5X,'CVZ =',E12.4,5X,'CDZ =',E12.4,I5)
          call LINER    (1,LU)
          call PRIVET   (LU,V,N)
          if(MFMV.gt.0) then
            call LINER  (1,LU)
            write (LU,102)
  102       format(' ','Multiplier = [ 1 - tanh(x) ] / 2, where x = ',
     $                 '(Z -CVZ) / CDZ')
            call LINER  (1,LU)
            call PRIVET (LU,FMV,N)
          end if
        end if
      end if
C     !END
      call BYE ('KALMIA')
C
      return
      end
