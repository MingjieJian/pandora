      subroutine PALDAO
     $(EP1,NE,P,S,EP2,F1,RNDT,XLF,W,IW,KODE,DUMP)
C
C     Rudolf Loeser, 1968 Aug 06
C---- Computes S for TAURUS.
C     !DASH
      save
C     !DASH
      real*8 EP1, EP2, F1, P, RNDT, S, W, XLF
      integer IW, KODE, NE
      logical DUMP
      character HEAD*80
C     !DASH
      external ALAPOD, LOPADA, ENGINE, ZERO1, MESHED, MASHED, HI, BYE
C
      dimension W(*), IW(*)
C
C               P(NE,NE), EP1(NE), EP2(NE), F1(NE), RNDT(NE), XLF(NE),
      dimension P(*),     EP1(*),  EP2(*),  F1(*),  RNDT(*),  XLF(*),
C
C               S(NE)
     $          S(*)
C
      data HEAD /'Final matrix for Lyman Source Function'/
C
      call HI ('PALDAO')
C     !BEG
C---- Compute matrix of integrals P
      call ALAPOD   (NE, F1, EP1, P)
C
C---- Invert (and print ?) matrix P
      if(DUMP) then
        call MESHED ('PALDAO', 2)
      end if
      call ENGINE   (P, W, IW, NE, HEAD, DUMP, KODE)
      if(DUMP) then
        call MASHED ('PALDAO')
      end if
C
C---- Compute S
      if(KODE.gt.0) then
        call LOPADA (NE, F1, EP1, EP2, RNDT, XLF, P, S)
      else
        call ZERO1  (S, NE)
      end if
C     !END
      call BYE ('PALDAO')
C
      return
      end
