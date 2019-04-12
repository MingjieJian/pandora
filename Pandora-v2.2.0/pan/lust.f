      subroutine LUST
     $(N,H2N,HND,HELABD,XNE,TE,GDENS,PEL,PGS,PTU,PEX,PTO,PMG)
C
C     Rudolf Loeser, 1987 Sep 11
C---- Prints error data and aborts, for GHASTLY.
C     !DASH
      save
C     !DASH
      real*8 GDENS, H2N, HELABD, HND, PEL, PEX, PGS, PMG, PTO, PTU, TE,
     $       XNE
      integer I, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, LINER, ABORT, HI, BYE
C
C               H2N(N), HND(N), XNE(N), PGS(N), PTU(N), PEX(N), PTO(N),
      dimension H2N(*), HND(*), XNE(*), PGS(*), PTU(*), PEX(*), PTO(*),
C
C               PEL(N), TE(N), GDENS(N), HELABD(N), PMG(N)
     $          PEL(*), TE(*), GDENS(*), HELABD(*), PMG(*)
C
      call HI ('LUST')
C     !BEG
      call MESHED ('LUST', 1)
      write (LUEO,100)
  100 format(' ','Total Pressure is negative.')
      call LINER  (2, LUEO)
      write (LUEO,101)
  101 format(' ',12X,'H2N',7X,'HND',4X,'HELABD',7X,'XNE',8X,'TE',5X,
     $           'GDENS',7X,'PEL',6X,'PGS',7X,'PTU',7X,'PEX',7X,'PTO'
     $           7X,'PMG')
      call LINER  (1, LUEO)
      write (LUEO,102) (I,H2N(I),HND(I),HELABD(I),XNE(I),TE(I),GDENS(I),
     $                    PEL(I),PGS(I),PTU(I),PEX(I),PTO(I),PMG(I),
     $                  I=1,N)
  102 format(5(' ',I5,1P12E10.2/))
      call ABORT
C     !END
      call BYE ('LUST')
C
      return
      end
