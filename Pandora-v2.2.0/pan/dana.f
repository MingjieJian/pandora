      subroutine DANA
     $(N,Z,TE,XNE,PGS,XION,RHEAB,HEK,HE1,HE2K,HE21,ZION,DEE,DEEL,
     $ VEC,W,IW,DUMP)
C
C     Rudolf Loeser, 1990 Jul 03
C---- Computes the d-coefficients, for the diffusion calculation.
C     !DASH
      save
C     !DASH
      real*8 DEE, DEEL, FACTOR, FZION, HE1, HE21, HE2K, HEK, PGS, RFHEA,
     $       RHEAB, TE, VEC, W, XION, XNE, Z, ZION
      integer I, IDFDM, IDFDS, IW, N
      logical DUMP
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(111),IDFDM)
      equivalence (RZQ(116),RFHEA)
      equivalence (RZQ(128),FZION)
      equivalence (KZQ(203),IDFDS)
C     !DASH
      external HATHOR, PEGEL, HALT, CONMUL, NADA, THORAH, HI, BYE
C
      dimension W(*), IW(*)
C
C               XION(N), HEK(N), HE1(N), HE2K(N), DEE(4,5,N), RHEAB(N),
      dimension XION(*), HEK(*), HE1(*), HE2K(*), DEE(4,5,*), RHEAB(*),
C
C               HE21(N), ZION(N), VEC(N), PGS(N), XNE(N), DEEL(4,5,N),
     $          HE21(*), ZION(*), VEC(*), PGS(*), XNE(*), DEEL(4,5,*),
C
C               TE(N), Z(N)
     $          TE(*), Z(*)
C     !EJECT
C
      call HI ('DANA')
C     !BEG
      if(IDFDM.eq.0) then
C----   The "original" method
        call HATHOR (N, TE, PGS, XION, DEE)
C
      else if(IDFDM.eq.1) then
C----   The "improved" method
        call PEGEL  (N, TE, XNE, PGS, XION, RHEAB, HEK, HE1, HE2K,
     $               HE21, DEE, W, IW, DUMP)
        if(DUMP) then
          call NADA (N, TE, XNE, PGS, XION, RHEAB, HEK, HE1, HE2K,
     $               HE21, DEE, DEEL, RFHEA, FZION, ZION)
        end if
C
      else
        write (MSSLIN(1),100) IDFDM
  100   format('IDFDM =',I12,', which is not 0 or 1.')
        call HALT   ('DANA', 1)
      end if
C
      if(IDFDS.gt.0) then
C----   Smoothing
        call THORAH (N, Z, DEE, VEC, DEEL, W, IW, DUMP)
      end if
C
C---- Multiply by ZION and RFHEA
      do 101 I = 1,N
        FACTOR = FZION*ZION(I)
C
        call CONMUL (FACTOR, DEE(1,1,I), 20)
C
        DEE(2,1,I) = DEE(2,1,I)*RFHEA
        DEE(2,3,I) = DEE(2,3,I)*RFHEA
        DEE(2,4,I) = DEE(2,4,I)*RFHEA
        DEE(2,5,I) = DEE(2,5,I)*RFHEA
  101 continue
C     !END
      call BYE ('DANA')
C
      return
      end
