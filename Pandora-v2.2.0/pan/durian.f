      subroutine DURIAN
     $(X,W,IW,TNU,OPAC,Z,EMUX,XK,KK,AK,Y,GK,V,XLB,D,F1,P,NE,NZ,WN,
     $ IXUSE,GP,IMG,EXT,DUMP,DMPW)
C
C     Rudolf Loeser, 1968 Aug 06
C---- Computes the matrix P, and auxiliary arrays.
C
C     (Note: NZ equals N; NE (i.e. "n to eta") is the length of a
C     reduced set, not greater than N.)
C
C     (This is version 2 of DURIAN.)
C     !DASH
      save
C     !DASH
      real*8 AK, D, EMUX, EXT, F1, GK, GP, OPAC, P, TNU, V, W, WN, X,
     $       XK, XKK, XLB, Y, Z, dummy
      integer IMG, IW, IXUSE, K, KK, LUEO, NE, NZ
      logical DDER, DMPW, DUMP, MOVING
      character TITLE*100
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external BLAB, MASHED, BLOB, ARROUT, DERVISH, PACO, LINER, PELYX,
     $         BLUB, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               D(NE), TNU(NZ,KKX), OPAC(NZ,KKX), P(NE,NE), IXUSE(KKX),
      dimension D(*),  TNU(NZ,*),   OPAC(NZ,*),   P(*),     IXUSE(*),
C
C               EXT(NE), V(NZ,KKX), XLB(NZ,KKX), WN(NZ,NZ,KKX), IMG(N),
     $          EXT(*),  V(NZ,*),   XLB(NZ,*),   WN(NZ,NZ,*),   IMG(*),
C
C               XK(KKX), AK(KKX),  GK(KKX), F1(N), EMUX(NZ,KKX), Z(NZ),
     $          XK(*),   AK(*),    GK(*),   F1(*), EMUX(NZ,*),   Z(*),
C
C               GP(NE,NE)
     $          GP(*)
C
      data MOVING,DDER /.false., .false./
C
      call HI ('DURIAN')
C     !BEG
      if(DUMP) then
        call BLAB     (NE, NZ, KK, EMUX, V, 'DURIAN')
      end if
C
C---- Loop over all frequencies.
      do 101 K = 1,KK
        XKK = XK(K)
C----   Compute weight matrix.
        write (TITLE,100) K,XKK
  100   format(' ','Weight matrix for Lyman frequency',I8,',',1PE20.12)
        call DERVISH  (X, W, IW, NE, TNU(1,K), OPAC(1,K), Z, Y,
     $                 MOVING, WN(1,1,K), dummy, 0, TITLE, DDER,
     $                 IXUSE(K), IMG)
        if(IXUSE(K).eq.1) then
C----     Calculate and add current increment to P(Lambda-1).
          call PACO   (NE, K, XKK, AK(K), GK(K), EMUX(1,K), EXT,
     $                 V(1,K), XLB(1,K), WN(1,1,K), GP, P)
          if(DUMP) then
            call BLOB (K, XKK, NE, WN(1,1,K), GP, DMPW)
          end if
        else
C----     Print error message
          call BLUB   (K)
        end if
  101 continue
C
C---- Convert P(Lambda-1) to P(Lambda)
      call PELYX      (D, F1, P, NE)
      if(DUMP) then
        call ARROUT   (LUEO, P, NE, NE, 'P')
        call MASHED   ('DURIAN')
      end if
C     !END
      call BYE ('DURIAN')
C
      return
      end
