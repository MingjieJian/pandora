      subroutine HUDRY
     $(NSL,XNU,XNUC,MRJ,RRNUIJ,WRATIJ,RWKSI)
C
C     Rudolf Loeser, 1974 Mar 04
C---- Converts Wavelengths to ratios of Frequnits, and
C     sets up continuum edge wavelengths in WRATIJ, and
C     sets up RWKSI.
C     (This is version 2 of HUDRY.)
C     !DASH
      save
C     !DASH
      real*8 ONE, RRNUIJ, RWKSI, WRATIJ, XNU, XNUC
      integer I, IR, J, MRJ, NOION, NSL, jummy
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
      equivalence (KZQ( 94),NOION)
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
      external MINK, NOMAD, DAMON, HI, BYE
C
C               MRZ = MRS+NSL+1
C
C               RRNUIJ(MRZ), MRJ(NSL+1), WRATIJ(MRZ), XNUC(NSL,)
      dimension RRNUIJ(*),   MRJ(*),     WRATIJ(*),   XNUC(*),
C
C               XNU(NSL)
     $          XNU(*)
C
      call HI ('HUDRY')
C     !BEG
      if(NOION.le.0) then
C
        do 101 J = 1,(NSL+1)
          call MINK      (J, MRJ, jummy, IR)
C
          if(J.gt.NSL) then
            RWKSI = WRATIJ(IR)
          end if
C
          if(WRATIJ(IR).eq.ONE) then
            call NOMAD   (ONE, XNU, XNUC, J, WRATIJ(IR))
            RRNUIJ(IR) = ONE
          else
            call DAMON   (WRATIJ(IR), XNU, XNUC, J, RRNUIJ(IR))
          end if
C
C
          if(MRJ(J).gt.0) then
            do 100 I = 1,MRJ(J)
              IR = IR+1
              call DAMON (WRATIJ(IR), XNU, XNUC, J, RRNUIJ(IR))
  100       continue
          end if
  101   continue
C
      end if
C     !END
      call BYE ('HUDRY')
C
      return
      end
