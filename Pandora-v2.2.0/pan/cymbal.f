      subroutine CYMBAL
     $(LU,N,NL,NSL,XNK,XNKS,RK,XND,XNDS,RN,BDI,FION,FLVS,FIONL,FLVSL,
     $ IKS,INS,IBS,NBAD,EDITED,OFCE)
C     Rudolf Loeser, 1975 Jul 30
C---- Prints number densities.
C     !DASH
      save
C     !DASH
      real*8 BDI, FION, FIONL, FLVS, FLVSL, RK, RN, XND, XNDS, XNK,
     $       XNKS
      integer I, IB, IBS, IE, IKS, INS, J, LU, N, NL, NSL
      logical EDITED, LONG, NBAD, OFCE
      character DASH*12, SIGNAL*1, TIT*7
C     !DASH
      external  ALAR, EBISSA, RETHO, LINER, HI, BYE
      intrinsic min
C
C               XNDS(N,NL), XNKS(N), RK(N), FION(N), FLVS(N), FIONL(N),
      dimension XNDS(N,*),  XNKS(*), RK(*), FION(*), FLVS(*), FIONL(*),
C
C               XND(N,NL), RN(N,NL), BDI(N,NL), INS(N,NL), IBS(N,NL),
     $          XND(N,*),  RN(N,*),  BDI(N,*),  INS(N,*),  IBS(N,*),
C
C               IKS(N), FLVSL(N), XNK(N)
     $          IKS(*), FLVSL(*), XNK(*)
C
      dimension TIT(11), SIGNAL(3)
C
      data      TIT /'N(ION)', 'N(ION)*', 'R(ION)', ' ', ' ',
     $               ' ', ' ', 'FION', 'FLVS', 'FIONL', 'FLVSL'/
      data      DASH /'------------'/
      data      SIGNAL /'_', ' ', '^'/
C
      call HI ('CYMBAL')
C     !BEG
      if(LU.gt.0) then
C----   Print header
        call EBISSA (LU, NL, NSL, LONG, NBAD, EDITED, OFCE)
C----   Set codes for signals
        call ALAR   (XNK, N, 1 , IKS)
        call ALAR   (XND, N, NL, INS)
        if(LONG) then
          call ALAR (BDI, N, NL, IBS)
        end if
C     !EJECT
C----   Print tables
        IE = 0
  100   continue
          IB  = IE+1
          IE  = min(IE+10,N)
          call LINER     (2, LU)
          write (LU,101) (DASH,I=IB,IE)
  101     format(' ',7X,10A12)
          write (LU,102) (I,I=IB,IE)
  102     format(' ','Depth ',10I12)
          call LINER     (1, LU)
          write (LU,104) TIT(1),(XNK(I),SIGNAL(IKS(I)+2),I=IB,IE)
  103     format(' ',A7,1P,10(E11.4,1X))
  104     format(' ',A7,1P,10(E11.4,A1))
          if(LONG) then
            write (LU,103) TIT(2),(XNKS(I),I=IB,IE)
            write (LU,103) TIT(3),(RK(I),I=IB,IE)
          end if
          do 105 J = 1,NL
            call RETHO   (J, TIT(4), TIT(5), TIT(6), TIT(7))
            if(LONG) then
              call LINER (1, LU)
              write (LU,104) TIT(4),(XND(I,J) ,SIGNAL(INS(I,J)+2),
     $                               I=IB,IE)
              write (LU,103) TIT(5),(XNDS(I,J),I=IB,IE)
              write (LU,103) TIT(6),(RN  (I,J),I=IB,IE)
              write (LU,104) TIT(7),(BDI(I,J) ,SIGNAL(IBS(I,J)+2),
     $                               I=IB,IE)
            else
              write (LU,104) TIT(4),(XND(I,J) ,SIGNAL(INS(I,J)+2),
     $                               I=IB,IE)
            end if
  105     continue
          if(LONG) then
            call LINER   (1, LU)
            write (LU,103) TIT( 8),(FION(I) ,I=IB,IE)
            write (LU,103) TIT( 9),(FLVS(I) ,I=IB,IE)
            call LINER   (1, LU)
            write (LU,103) TIT(10),(FIONL(I),I=IB,IE)
            write (LU,103) TIT(11),(FLVSL(I),I=IB,IE)
          end if
        if(IE.lt.N) goto 100
      end if
C     !END
      call BYE ('CYMBAL')
C
      return
      end
