      subroutine BONDA
     $(HYDRO,NL,NPQ,LRQ,LCX,KODE)
C
C     Rudolf Loeser, 1990 Nov 21
C---- Sets up "charge exchange level" markers, and
C     sets KODE=1 if there are any.
C     !DASH
      save
C     !DASH
      integer I, KODE, LCX, LRQ, NL, NPQ
      logical HYDRO, NGTL
C     !COM
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
C     !DASH
C     !EJECT
      external  HI, BYE
      intrinsic min, max
C
C               NPQ(NL), LRQ(NL), LCX(NL)
      dimension NPQ(*),  LRQ(*),  LCX(*)
C
      call HI ('BONDA')
C     !BEG
      KODE = 0
C
      if(HYDRO) then
        do 100 I = 1,NL
          if((I.ge.4).and.(I.le.NPQLM)) then
            LCX(I) = 1
            KODE   = 1
          else
            LCX(I) = 0
          end if
  100   continue
        NPQMX = min(NL,NPQLM)
      else
C
        NPQMX = 0
        do 101 I = 1,NL
          NGTL = NPQ(I).gt.LRQ(I)
          if((LRQ(I).eq.(-1)).or.((LRQ(I).ge.3).and.NGTL)) then
            LCX(I) = 1
            KODE   = 1
          else
            LCX(I) = 0
          end if
          NPQMX = max(NPQMX,NPQ(I))
  101   continue
      end if
C     !END
      call BYE ('BONDA')
C
      return
      end
