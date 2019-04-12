      subroutine DILMUN
     $(N,VM,V1,V2,V3,HE1,BETA,HE2K,HECHECK)
C
C     Rudolf Loeser, 1991 Jan 04
C---- Computes and prints HECHECK, for diffusion calculations.
C     !DASH
      save
C     !DASH
      real*8 BETA, CHEFL, D, HE1, HE2K, HECHECK, ONE, T, T1, T2, T3, TF,
     $       TM, V1, V2, V3, VM, Z
      integer I, JHEAB, LUEO, N
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
      equivalence (RZQ(100),CHEFL)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
      equivalence (LEST(59),JHEAB)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  DIVIDE, LINER, PRIVET, VECOUT, HI, BYE
      intrinsic max, abs
C
C               VM(N), V1(N), V2(N), V3(N), BETA(N), HE1(N), HECHECK(N),
      dimension VM(*), V1(*), V2(*), V3(*), BETA(*), HE1(*), HECHECK(*),
C
C               HE2K(N)
     $          HE2K(*)
C     !EJECT
C
      call HI ('DILMUN')
C     !BEG
      if(JHEAB.gt.0) then
C
        TF = abs(CHEFL)
        do 100 I = 1,N
          T1 = V1(I)*HE1(I)
          T2 = V2(I)*BETA(I)
          T3 = V3(I)*HE2K(I)
          TM = VM(I)*(HE1(I)+BETA(I)+HE2K(I))
          D  = max(TF,abs(T1),abs(T2),abs(T3),abs(TM))
          T  = T1+T2+T3+TM-CHEFL
          call DIVIDE (T, D, Z)
          HECHECK(I) = ONE-Z
  100   continue
C
        call LINER    (3, LUEO)
        write (LUEO,101)
  101   format(' ','HECHECK, a measure of the accuracy of the ',
     $             'recalculation of RHEAB')
        call PRIVET   (LUEO, HECHECK, N)
        call VECOUT   (LUEO, VM, N, 'VM, recomputed')
C
      end if
C     !END
      call BYE ('DILMUN')
C
      return
      end
