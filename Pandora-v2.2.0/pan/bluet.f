      subroutine BLUET
     $(N,BDI,XND,XNK,ESG)
C
C     Rudolf Loeser, 1988 Aug 03
C---- Dumps, for BOWMAN
C     !DASH
      save
C     !DASH
      real*8 BDI, ESG, XND, XNK
      integer I, LUEO, N
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external TULBE, LINER, MASHED, HI, BYE
C
C               XND(N,NL), XNK(N), BDI(N,NL), ESG(N)
      dimension XND(N,*),  XNK(*), BDI(N,*),  ESG(*)
C
      call HI ('BLUET')
C     !BEG
      call TULBE    ('BOWMAN', DUMP)
      if(DUMP) then
        write (LUEO,100)
  100   format(' ',4X,'i',12X,'NK',13X,'ESG',14X,'N1',13X,'BD1')
        call LINER  (1,LUEO)
        write (LUEO,101) (I,XNK(I),ESG(I),XND(I,1),BDI(I,1),I=1,N)
  101   format(5(' ',I5,1P4E16.8/))
        call MASHED ('BOWMAN')
      end if
C     !END
      call BYE ('BLUET')
C
      return
      end
