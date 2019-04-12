      subroutine RHEIN
     $(X,IX,W,IW,N,NL,BDI,RHOIJ,YBRIJ,CIJ,GMI,CQUI,CQSI,GVL)
C
C     Rudolf Loeser, 1988 Jul 28
C---- Computes a set of b-values (departure coefficients),
C     using "all level equations".
C     (This is version 2 of RHEIN.)
C     !DASH
      save
C     !DASH
      real*8 BDI, CIJ, CQSI, CQUI, GMI, GVL, RHOIJ, W, X, YBRIJ
      integer IB, IN, IS, ITAU, IW, IX, IXM, IXMS, KODE, KRJ, LUEO, MOX,
     $        N, NL
      logical DUMP
      character TIT*50
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external RUHR, NECKAR, WAVERLY, LAHN, MOSEL, MOVE1, MOVED, MOTOR,
     $         ONE1, ARROUT, DARROUT, TULBE, MESHED, MASHED, WGIVE,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               CIJ(N,NL,NL), RHOIJ(N,NT), GMI(N,NSL), YBRIJ(N,NT),
      dimension CIJ(*),       RHOIJ(*),    GMI(*),     YBRIJ(*),
C
C               CQUI(N,NSL), CQSI(N,NSL), BDI(N,NL), GVL(N,NL)
     $          CQUI(*),     CQSI(*),     BDI(N,*),  GVL(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IB    ),(IN( 2),IXM   ),(IN( 3),IXMS  )
C     !EJECT
C
      call HI ('RHEIN')
C     !BEG
C     (Get, and allocate, W allotment)
      call MOSEL         (IN, IS, MOX, 'RHEIN')
C
      call TULBE         ('RHEIN', DUMP)
      call WAVERLY       (IX, YBRIJ, KRJ)
C
      do 101 ITAU = 1,N
        if(DUMP) then
          call RUHR      (ITAU)
        end if
C----   Compute matrix M
        call NECKAR      (ITAU, N, NL, KRJ, X, IX, CIJ, RHOIJ, YBRIJ,
     $                    GMI, CQUI, GVL, W(IXM), DUMP)
C----   Invert M
        write (TIT,100) ITAU
  100   format('Matrix M for b calculation at depth',I4)
        call MOVE1       (W(IXM), (NL**2), W(IXMS))
        call MOTOR       (W(IXM), NL, TIT, W, IW, KODE)
        if(KODE.le.0) then
C----     Inversion failed
          if(.not.DUMP) then
            call MESHED  ('RHEIN', 3)
            call DARROUT (LUEO, W(IXMS), NL, NL, 'Original Matrix')
            call MASHED  ('RHEIN')
          end if
C----     Set default b's
          call ONE1      (W(IB), NL)
        else
          if(DUMP) then
            call ARROUT  (LUEO, W(IXM), NL, NL, 'M-inverse')
          end if
C----     Compute b's
          call LAHN      (ITAU, N, NL, W(IXM), CQSI, W(IB))
        end if
C----   Move to final resting place
        call MOVED       (W(IB), 1, NL, BDI(ITAU,1), N, NL)
  101 continue
      if(DUMP) then
        call MASHED      ('RHEIN')
      end if
C
C     (Give back W allotment)
      call WGIVE         (W, 'RHEIN')
C     !END
      call BYE ('RHEIN')
C
      return
      end
