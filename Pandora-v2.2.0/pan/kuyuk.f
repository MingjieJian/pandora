      subroutine KUYUK
     $(X,MODE,BDU,BDL,TE,SET)
C
C     Rudolf Loeser, 2004 May 19
C---- Computes SET: the stimulated emission term.
C
C     MODE = 1 means: X is in frequency units
C          = 2      : X is in Angstroms
C
C     (This is version 4 of KUYUK.)
C     !DASH
      save
C     !DASH
      real*8 BDL, BDU, BRAT, SET, TE, TERM, X, dummy
      integer MODE
C     !DASH
      external DIVIDE, PROD, HI, BYE
C
      call HI ('KUYUK')
C     !BEG
      call DIVIDE (BDU, BDL, BRAT)
      call PROD   (TE, X, MODE, dummy, TERM)
C
      SET = BRAT*TERM
C     !END
      call BYE ('KUYUK')
C
      return
      end
