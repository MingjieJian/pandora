      subroutine LINDEN
     $(W,IW,N,JJ,DMP1,EP,BS,PA,PB,DEL,DLC,FNDT,BF,BA,EM,SN,IMG,S,EDITS,
     $ KODE)
C
C     Rudolf Loeser, 1977 Jul 01
C           revised, 2004 May 07
C
C---- Computes S by the FULL method, either
C     the entire table (JJ = N), or an outer section of it (JJ < N).
C
C---- (This is version 2 of LINDEN.)
C     !DASH
      save
C     !DASH
      real*8 BA, BF, BS, DEL, DLC, EM, EP, FNDT, PA, PB, S, SN, W
      integer IMG, IW, JJ, KODE, N
      logical DMP1, EDITS
      character TIT*80
C     !DASH
      external ENGINE, BAKOTO, KURTU, PETREL, ZERO1, MUMBLE, HI, BYE
C
      dimension W(*), IW(*)
C
C               EP(N), BS(N), FNDT(N), EM(N,N), PA(N,N), IMG(N), BA(N),
      dimension EP(*), BS(*), FNDT(*), EM(*),   PA(*),   IMG(*), BA(*),
C
C               PB(N), BF(N), DLC(N), SN(N), S(N), DEL(N)
     $          PB(*), BF(*), DLC(*), SN(*), S(*), DEL(*)
C
      call HI ('LINDEN')
C     !BEG
C---- Compute BA and BF
      call PETREL   (N, EP, BS, PB, DEL, DLC, FNDT, BA, BF)
C
C---- Compute matrix M
      call KURTU    (N, EM, EP, DEL, PA)
C---- Invert (and print ?) matrix M
      call MUMBLE   (TIT)
      call ENGINE   (EM, W, IW, N, TIT, DMP1, KODE)
C
      if(KODE.eq.1) then
C----   Compute S
        call BAKOTO (W, JJ, N, EM, BF, SN, IMG, S, EDITS)
      else
C----   Matrix calculation failed. set S=0
        call ZERO1  (S, N)
      end if
C     !END
      call BYE ('LINDEN')
C
      return
      end
