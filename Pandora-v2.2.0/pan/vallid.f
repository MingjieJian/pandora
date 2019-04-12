      subroutine VALLID
     $(IL,IG,N,LABEL,KODE)
C
C     Rudolf Loeser, 1979 Dec 02
C---- Checks the processability of a bad interval, for SENTA.
C     IL  is the index of last good value preceeding the bad ones,
C     IG  is the index of first good value succeeding the bad ones.
C
C     Returns with: KODE=1 if no interval found;
C                       =2 if interval is processable; or
C                       =3 if interval is not processable.
C     !DASH
      save
C     !DASH
      integer IG, IL, J, KODE, LUEO, N
      character LABEL*(*), WORD*6
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MESHED, MASHED, LINER, HI, BYE
C
      dimension WORD(3)
C
      data WORD /'is all', 'starts', 'ends'/
C
      call HI ('VALLID')
C     !BEG
      if((IG-IL).gt.N) then
        KODE = 3
        J = 1
        goto 100
      end if
      if(IL.ge.N) then
        KODE = 1
        goto 103
      end if
      if(IL.le.0) then
        KODE = 3
        J = 2
        goto 100
      end if
      if(IG.gt.N) then
        KODE = 3
        J = 3
        goto 100
      end if
C
      KODE = 2
      goto 103
C
  100 continue
      call MESHED ('VALLID', 3)
      write (LUEO,101) LABEL
  101 format(' ','Editing negatives out of a function of TAU.'/
     $       ' ',A)
      call LINER  (1, LUEO)
      write (LUEO,102) WORD(J)
  102 format(' ','"Old" ',A,' bad --- no editing.')
      call MASHED ('VALLID')
C
  103 continue
C     !END
      call BYE ('VALLID')
C
      return
      end
