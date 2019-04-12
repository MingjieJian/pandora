      subroutine HULCA
     $(N,IQVLG,HEABD,RHEAB,ASTAR,DEE,ZI,Z2,Z3,ZT,CUE,HEMSS,ELL,CVM,HND,
     $ CAY,Z,WHY,ZR,FR,YR,IHEAB)
C
C     Rudolf Loeser, 1991 Jan 04
C---- Computes a*, Q, K, and y for THALIA.
C     !DASH
      save
C     !DASH
      real*8 ASTAR, C, CAY, CMPKM, CUE, CVM, DEE, ELL, FR, HEABD, HEMSS,
     $       HND, ONE, RHEAB, TB, TV, WHY, XNUM, YR, Z, Z2, Z3, ZERO,
     $       ZI, ZR, ZT
      integer I, IHEAB, IQVLG, N
      logical VEL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 5),CMPKM )
C     !DASH
      external URANIA, DIVIDE, WATER, CATRIN, HI, BYE
C
C               DEE(4,5,N), ZI(N), Z2(N), Z3(N), ZT(N), RHEAB(N), Z(N),
      dimension DEE(4,5,*), ZI(*), Z2(*), Z3(*), ZT(*), RHEAB(*), Z(*),
C
C               CUE(N), ELL(N), HND(N), CAY(N), ZR(IHEAB), FR(IHEAB),
     $          CUE(*), ELL(*), HND(*), CAY(*), ZR(*),     FR(*),
C
C               WHY(N), YR(IHEAB)
     $          WHY(*), YR(*)
C
      data C /1.D10/
C     !EJECT
C
      call HI ('HULCA')
C     !BEG
      VEL = (IQVLG.gt.0).and.(CVM.ne.ZERO)
      if(VEL) then
        call URANIA (N,HND,RHEAB,HEABD,ASTAR)
        XNUM = (ONE+HEMSS*ASTAR)*CVM*C*CMPKM
      else
        ASTAR = ZERO
        TV = ZERO
      end if
C
      do 100 I = 1,N
        CUE(I) = DEE(2,1,I)*ZI(I)+DEE(2,3,I)*Z2(I)+
     $           DEE(2,4,I)*Z3(I)+DEE(2,5,I)*ZT(I)
        if(VEL) then
          TV = XNUM/HND(I)
        end if
        call DIVIDE ((CUE(I)-TV),DEE(2,2,I),TB)
        CAY(I) = HEMSS*ELL(I)+TB
  100 continue
C
      call WATER    (Z,CAY,WHY,N,ZR,FR,YR,IHEAB)
      call CATRIN   (WHY,N)
C     !END
      call BYE ('HULCA')
C
      return
      end
