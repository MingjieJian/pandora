      subroutine WHALE
     $(N,R,BB,WN,XM,W,IW,XLM,DUMP,S,KODE)
C
C     Rudolf Loeser, 1980 Nov 04
C---- Computes S by matrix method, for SASKIA.
C     Returns with KODE=1 if all seems ok, =0 if not.
C     (This is version 2 of WHALE.)
C     !DASH
      save
C     !DASH
      real*8 BB, R, S, W, WN, XLM, XM
      integer IW, KODE, N
      logical DUMP
      character HEAD*80
C     !DASH
      external WUMP, MIMBLE, ENGINE, JADE, HI, BYE
C
      dimension W(*), IW(*)
C
C               R(N), BB(N), WN(N,N), XM(N,N), S(N)
      dimension R(*), BB(*), WN(*),   XM(*),   S(*)
C
      call HI ('WHALE')
C     !BEG
C---- Compute matrix.
      call WUMP   (N, R, WN, XM)
C
C---- Invert (and print ?) matrix.
      call MIMBLE (XLM, HEAD)
      call ENGINE (XM, W, IW, N, HEAD, DUMP, KODE)
C
      if(KODE.eq.1) then
C----   Compute S.
        call JADE (N, XM, BB, S)
      end if
C     !END
      call BYE ('WHALE')
C
      return
      end
