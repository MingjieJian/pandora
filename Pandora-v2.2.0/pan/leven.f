      subroutine LEVEN
     $(IU,IL,N,K,LENGTH,CALLER,TYPE)
C
C     Rudolf Loeser, 1982 Sep 22
C---- Dumps, for VULCAN.
C     (This is version 3 of LEVEN.)
C     !DASH
      save
C     !DASH
      integer IL, IU, K, LENGTH, LUEO, N
      character CALLER*(*), HEAD*80, TYPE*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external GENIE, MESHED, LINER, HI, BYE
C
      call HI ('LEVEN')
C     !BEG
      call MESHED (CALLER, 2)
      call LINER  (3, LUEO)
      call GENIE  (HEAD)
      write (LUEO,100) HEAD
  100 format(' ',A)
      call LINER  (2, LUEO)
      write (LUEO,101) TYPE,IU,IL,N,K,LENGTH
  101 format(' ','Detailed dump, preparation of ',A,'(frequency sum) ',
     $           'Data Blocks.'//
     $       ' ','(',I2,'/',I2,')',5X,'N =',I4,5X,'K =',I4,5X,
     $           'Block length =',I10)
C     !END
      call BYE ('LEVEN')
C
      return
      end
