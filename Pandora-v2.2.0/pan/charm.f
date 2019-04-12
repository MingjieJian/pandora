      subroutine CHARM
     $(N,NL,NSL,NO,GM,RL,CK,TE,SA,HND,XNE,SQCA,SQRA)
C
C     Rudolf Loeser, 1976 Nov 04
C---- Prints recombination data, for SALLY.
C     (This is version 2 of CHARM.)
C     !DASH
      save
C     !DASH
      real*8 CK, DRCT, GM, HND, RL, SA, SQCA, SQRA, TE, TQS, XNE
      integer I, IQPRE, L, LIM, LU, N, NIM, NL, NO, NSL
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(155),IQPRE)
C
C---- RAGU        as of 2000 Mar 02
      real*8      RASAHA,RASQR,RASQRA,RASQC,RASQCA,RARRC,RACRR,RADRC,
     $            RAWRR,RADEP,RAH,RATSQR,RATSQC,RASQS,RAX
      common      /RAGU/ RASAHA,RASQR,RASQRA,RASQC,RASQCA,RARRC,RACRR,
     $                   RADRC,RAWRR,RADEP,RATSQR,RATSQC,RASQS,RAH,RAX
C     Intermediate results of recombination calculation.
C     .
C     !DASH
      external  ZEUS, BUGLE, RAGOUT, SHIM, LINER, PRIVET, HI, BYE
C
C               HND(N), XNE(N), GM(N,NSL), RL(N,NSL), CK(N,NSL), TE(N),
      dimension HND(*), XNE(*), GM(*),     RL(*),     CK(*),     TE(*),
C
C               SA(N), SQCA(N), SQRA(N)
     $          SA(*), SQCA(*), SQRA(*)
C     !EJECT
C
      call HI ('CHARM')
C     !BEG
      call ZEUS         (NO,IQPRE,LU)
      if(LU.gt.0) then
        LIM = 1
        if(NSL.gt.NL) then
          LIM = 2
        end if
C
        do 103 L = 1,LIM
          call BUGLE    (LU,L,LIM)
C
          NIM = NL
          if(L.eq.2) then
            NIM = NSL
          end if
          do 101 I = 1,N
            call RAGOUT (I,N,1,NL,NIM,TE,SA,GM,RL,CK,HND,XNE,TQS,DRCT)
C
            if(L.eq.2) then
              SQRA(I) = RASQRA
              SQCA(I) = RASQCA
             end if
C
            write (LU,100) I,TE(I),TQS,RASQS,RATSQC,RATSQR,RARRC,RACRR,
     $                     RAWRR,RAX,RAH,RADEP,RADRC,RASAHA
  100       format(' ',I3,1P3E10.3,1X,2E9.2,1X,2E9.2,0PF7.3,1PE9.2,1X,
     $                 2E9.2,E10.3,1X,E10.3)
            call SHIM   (I,5,LU)
  101     continue
C
          if(L.eq.2) then
            call LINER  (2,LU)
            write (LU,102) 'SQR'
  102       format(' ','Supplementary levels contribution to ',A)
            call PRIVET (LU,SQRA,N)
C
            call LINER  (2,LU)
            write (LU,102) 'SQC'
            call PRIVET (LU,SQCA,N)
          end if
  103   continue
C
      end if
C     !END
      call BYE ('CHARM')
C
      return
      end
