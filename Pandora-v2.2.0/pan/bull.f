      subroutine BULL
     $(QNAME,I1,I2,ZAUX,NAUX,LZM,VAUX,NZ,Z,VECTOR,W1,W2)
C
C     Rudolf Loeser, 1972 Feb 01
C---- Extra(inter)polates to Standard Z, for CREAM.
C     !DASH
      save
C     !DASH
      real*8 VAUX, VECTOR, W1, W2, Z, ZAUX
      integer I1, I2, KERR, LUEO, LZM, N, NAUX, NZ
      logical JZX, JZZ, WGHT
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external NAUGHTD, CARMEN, BUCKET, BUFFET, NUTMEG, LINER, MESHED,
     $         PUFF, ABORT, HI, BYE
C
C               ZAUX(LZM,NZM), VAUX(LZM), W2(N), VECTOR(N), W1(LZM),
      dimension ZAUX(LZM,*),   VAUX(*),   W2(*), VECTOR(*), W1(*),
C
C               Z(N)
     $          Z(*)
C     !EJECT
C
      call HI ('BULL')
C     !BEG
      KERR = 0
      WGHT = (QNAME.eq.'RHWT').or.(QNAME.eq.'RKWT').or.
     $       (QNAME.eq.'RHOWT').or.(QNAME.eq.'RKW')
C
      call NAUGHTD  (Z, 1, N, JZZ)
        if(JZZ) goto 202
C
      call NAUGHTD  (ZAUX(1,NAUX), 1, NZ, JZX)
        if(JZX) goto 201
C
      call PUFF     (ZAUX(1,NAUX), VAUX, NZ, Z, VECTOR, N, W1, W2,
     $               QNAME, I1, I2)
C
      if(QNAME.eq.'RHO') then
        call BUCKET (ZAUX(1,NAUX), NZ, Z, N, VECTOR)
      else if(WGHT) then
        call BUFFET (VECTOR, N)
      end if
C
      goto 199
C
C---- Errors
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED   ('BULL', 1)
      call NUTMEG   (LUEO, 2)
      write (LUEO,203) QNAME,I1,I2
  203 format(' ','Error while reading for: ',A10,2I5)
      call LINER    (1,LUEO)
      if(KERR.eq.1) then
        write (LUEO,1201)
 1201   format(' ','ZAUX-table undefined.')
      else
        write (LUEO,1202)
 1202   format(' ','Z-table undefined.')
      end if
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('BULL')
C
      return
      end
