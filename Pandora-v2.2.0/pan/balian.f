      subroutine BALIAN
     $(N,NL,XNK,XND,BAD,LUF)
C
C     Rudolf Loeser, 1994 Oct 05
C---- Checks to see whether all final number densities are > 0, and
C     sets signals if not.
C     !DASH
      save
C     !DASH
      real*8 XND, XNK
      integer LGT, LUF, N, NL, NNL, NO
      logical BAD, KOK, NOK
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external PLUSD, HI, BYE
C
C               XNK(N), XND(N,NL)
      dimension XNK(*), XND(*)
C
C
      call HI ('BALIAN')
C     !BEG
      call PLUSD (XNK,1,N  ,LGT)
      KOK = LGT.ge.N
C
      NNL = N*NL
      call PLUSD (XND,1,NNL,LGT)
      NOK = LGT.ge.NNL
C
      BAD = .not.(KOK.and.NOK)
      if(BAD) then
        LUF = NO
      end if
C     !END
      call BYE ('BALIAN')
C
      return
      end
