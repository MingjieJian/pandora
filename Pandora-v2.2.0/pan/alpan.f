      subroutine ALPAN
     $(N,NL,M,PKL,PLK1,GMI,XND,CQSI,CKI,RKI,GNVL)
C
C     Rudolf Loeser, 1990 Apr 12
C---- Computes levels-sums, for PONGO.
C     (This is version 2 of ALPAN.)
C     !DASH
      save
C     !DASH
      real*8 CKI, CQSI, GMI, GNVL, PKL, PLK1, RKI, TERM, XND, XNRAT
      integer I, J, LGT, M, N, NL
      logical NOK
C     !DASH
      external ZERO1, PLUSD, DIVIDE, HI, BYE
C
C               RKI(N,NSL), PLK1(N), GMI(N,NSL), XND(N,NL), GNVL(N,NL),
      dimension RKI(N,*),   PLK1(*), GMI(N,*),   XND(N,*),  GNVL(N,*),
C
C               CKI(N,NSL), CQSI(N,NSL), PKL(N)
     $          CKI(N,*),   CQSI(N,*),   PKL(*)
C
C
      call HI ('ALPAN')
C     !BEG
      call ZERO1          (PLK1,N)
      call ZERO1          (PKL ,N)
      call PLUSD          (XND(1,M), 1, N, LGT)
      NOK = LGT.eq.N
C
      do 101 J = 1,NL
        if(J.ne.M) then
C
          do 100 I = 1,N
C
            call DIVIDE   (CQSI(I,J), GMI(I,M), TERM)
            PKL(I) = PKL(I)+TERM
C
            if(NOK) then
              call DIVIDE (XND(I,J), XND(I,M), XNRAT)
              PLK1(I) = PLK1(I)+XNRAT*(RKI(I,J)+CKI(I,J)+GNVL(I,J))
            end if
C
  100     continue
C
        end if
  101 continue
C     !END
      call BYE ('ALPAN')
C
      return
      end
