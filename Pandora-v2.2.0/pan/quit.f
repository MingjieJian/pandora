      subroutine QUIT
     $(N,NL,NSL,L1,L2,GMI,RKI,RLI,CKI,TE,SA,HND,XNE,CQUI,CQSI,SQS,DRCT)
C
C     Rudolf Loeser, 1975 Jan 26
C---- Computes Ionization Terms, for SETTUP.
C     !DASH
      save
C     !DASH
      real*8 CKI, CQSI, CQUI, DRCT, GMI, HND, RKI, RLI, SA, SQS, TE,
     $       XNE
      integer J, L1, L2, N, NL, NSL
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external  CUTIE, LEDGE, ROWSUM, HALT, HI, BYE
C
C               GMI(N,NSL), RKI(N,NSL), RLI(N,NSL), CQUI(N,NSL), TE(N),
      dimension GMI(N,*),   RKI(N,*),   RLI(N,*),   CQUI(N,*),   TE(*),
C
C               CQSI(N,NSL), CKI(N,NSL), SA(N), HND(N), XNE(N), SQS(N),
     $          CQSI(N,*),   CKI(N,*),   SA(*), HND(*), XNE(*), SQS(*),
C
C               DRCT(N)
     $          DRCT(*)
C
      call HI ('QUIT')
C     !BEG
      if(L2.lt.L1) then
        write (MSSLIN(1),100) L1,L2
  100   format('L1 =',I12,', L2 =',I12,'; L1 .le. L2 is required.')
        call HALT    ('QUIT', 1)
      end if
C
C---- Update values of QU and QS (for levels L1 thru L2)
      do 101 J = L1,L2
        call CUTIE   (N, GMI(1,J), RKI(1,J), RLI(1,J), CKI(1,J),
     $                CQUI(1,J), CQSI(1,J))
        if(J.eq.1) then
C----     Compute and add dielectronic recombination to QS(1)
          call LEDGE (N, NL, NSL, TE, SA, GMI(1,J), RLI(1,J), CKI(1,J),
     $                HND, XNE, CQSI(1,J), DRCT)
        end if
  101 continue
C
C---- Compute sum over levels of QS = SQS
      call ROWSUM    (CQSI, N, N, 1, NL, SQS)
C     !END
      call BYE ('QUIT')
C
      return
      end
