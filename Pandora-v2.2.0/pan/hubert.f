      subroutine HUBERT
     $(LU,N,KWA,NKA,NT,NP,KNT,INDEX,ALB,WAVE,TE,XNE,GD,ALOMI,ALOMA,TT,
     $ PT,TAB,TABL,ARRL,WAVCA,ARRCA)
C
C     Rudolf Loeser, 1993 Sep 15
C---- Reads, massages and prints "averaged" line opacity data.
C     !DASH
      save
C     !DASH
      real*8 ALB, ALOMA, ALOMI, ARRCA, ARRL, GD, PT, TAB, TABL, TE, TT,
     $       WAVCA, WAVE, XNE
      integer INDEX, KNT, KWA, LU, LUKA, N, NKA, NP, NT
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(37),LUKA )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
C               INDEX(KNTKU), ALB(N), TE(N), XNE(N), GD(N), WAVCA(KWA),
      dimension INDEX(*),     ALB(*), TE(*), XNE(*), GD(*), WAVCA(*),
C
C               TABL(NT,NP), TAB(NT,NP), ARRL(KWA,KNRKU), ARRCA(KWA,N),
     $          TABL(*),     TAB(*),     ARRL(*),         ARRCA(*),
C
C               TT(NT), PT(NP)
     $          TT(*),  PT(*)
C
      call HI ('HUBERT')
C     !BEG
      write (MSSLIN(1),100)
  100 format('Custom reading, massaging, and printing of "averaged" ',
     $       'line opacity data must still be provided.')
      call HALT ('HUBERT',1)
C     !END
      call BYE ('HUBERT')
C
      return
      end
