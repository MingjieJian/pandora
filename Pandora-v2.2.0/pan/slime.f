      subroutine SLIME
     $(M,IUT,ILT,NAME,KODE,OK)
C
C     Rudolf Loeser, 2005 Jun 28
C---- Tells whether built-in transitions belong to the ion-of-the-run.
C     !DASH
      save
C     !DASH
      integer I, IL, ILT, IU, IUT, KODE, LUEO, M, NL, jummy
      logical OK
      character NAME*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external INDXNT, MASHED, MESHED, HI, BYE
C
C               IUT(M), ILT(M)
      dimension IUT(*), ILT(*)
C
      call HI ('SLIME')
C     !BEG
      do 100 I = 1,M
        OK = NL.ge.IUT(I)
        if(.not.OK) then
          goto 102
        end if
  100 continue
C
      do 101 I = 1,M
        call INDXNT (IUT(I), ILT(I), OK, jummy)
        if(.not.OK) then
          goto 102
        end if
  101 continue
      goto 104
C
  102 continue
      call MESHED   ('SLIME', 3)
      write (LUEO, 103) NAME
  103 format(' ',A,': ion-of-the-run and the built-in background ',
     $           'lines ion  model use different transitions.')
      call MASHED   ('SLIME')
      KODE = 0
C
  104 continue
C     !END
      call BYE ('SLIME')
C
      return
      end
