      program modup
C
C     Rudolf Loeser, 2002 Apr 11
C        modified for Oxygen, 2004 Mar 25, 30
C        modified for O-II,III, 2007 Jan 18
C
C---- Updates a model file from a given ---.pop file.
C     When MODE = 1, the .pop file came from a Hydrogen run;
C          MODE = 2, the .pop file came from another ion.
C
C---- Order of ions:
C
C     H    C    SI   HE   HE2  AL   MG   FE   NA   CA    O   O2   O3    S
C     1    2    3    4    5    6    7    8    9    10   11   12   13   14
C     !DASH
      save
C     !BDECL
      integer NI,NO,NP,K,KMX,L,LL,I,J
      character BUFF*80,STUFF*8
C     !EDECL
C     !COM
      include 'modup.inc'
C     !DASH
      external read_mod_sets, read_pop_sets, read_mod_stuff, buff_it,
     $         buff_out, init
C
      dimension BUFF(2000)
      data KMX /2000/
      data STUFF /'> UPDATE'/
C
      data MXL,MXT,MXI,MXM /100, 16, 14, 5/
C
      data ION /'H  ', 'C  ', 'SI ', 'HE ', 'HE2', 'AL ', 'MG ', 'FE ',
     $          'NA ', 'CA ', 'O  ', 'S  ', 'O2 ', 'O3 '/
C
      data NL /15, 8, 8, 13, 8, 8, 8, 8, 8, 8, 14, 8, 8, 8/
C
      data LABM /'NE   ', 'ZME  ', 'NC   ', 'BDHM ', 'NH   '/
C
      data LABK /'NP   ', 'CK   ', 'SIK  ', 'HEK  ', 'HE2K ', 'ALK  ',
     $           'MGK  ', 'FEK  ', 'NAK  ', 'CAK  ', 'OK   ', 'SK   ',
     $           'O2K  ', 'O3K  '/
      data LABN /'HN   ', 'CN   ', 'SIN  ', 'HEN  ', 'HE2N ', 'ALN  ',
     $           'MGN  ', 'FEN  ', 'NAN  ', 'CAN  ', 'ON   ', 'SN   ',
     $           'O2N  ', 'O3N  '/
      data LABB /'BDH  ', 'BDC  ', 'BDSI ', 'BDHE ', 'BDHE2', 'BDAL ',
     $           'BDMG ', 'BDFE ', 'BDNA ', 'BDCA ', 'BDO  ', 'BDS  ',
     $           'BDO2 ', 'BDO3 '/
C
      data NI,NO,NP /61, 62, 63/
C     !EJECT
C     !beg
      call init
C
      rewind NI
      call read_mod_stuff (NI,STUFF,BUFF,K,KMX)
      call read_mod_sets  (NI,LL)
      call read_pop_sets  (NP,LL)
C
      rewind NO
      call buff_out (NO,BUFF,K)
C
      K = 0
      do 101 J = 1,MXM
        call buff_it (HN(1),BUFF,K,KMX)
        do 100 L = 1,LL
          call buff_it (DM(L,J),BUFF,K,KMX)
  100   continue
  101 continue
      call buff_out (NO,BUFF,K)
C
      write (NO,102)
  102 format('USE ( INPUT ) > ')
C
      do 108 I = 1,MXI
C
        K = 0
        call buff_it (HN(I),BUFF,K,KMX)
        do 103 L = 1,LL
          call buff_it (DN(L,(NL(I)+1),I),BUFF,K,KMX)
  103   continue
        do 105 J = 1,NL(I)
          do 104 L = 1,LL
            call buff_it (DN(L,J,I),BUFF,K,KMX)
  104     continue
  105   continue
        call buff_out (NO,BUFF,K)
C
        K = 0
        call buff_it (HB(I),BUFF,K,KMX)
        do 107 J = 1,NL(I)
          do 106 L = 1,LL
            call buff_it (DB(L,J,I),BUFF,K,KMX)
  106     continue
  107   continue
        call buff_out (NO,BUFF,K)
C
  108 continue
C
      write (NO,102)
C
      stop 'modup: done'
C     !end
      end
      subroutine init
C
C     Rudolf Loeser, 2003 Aug 14
C     !DASH
      save
C     !BDECL
      integer I,J,K
      character EMPTY*5
C     !EDECL
C     !COM
      include 'modup.inc'
C     !DASH
      data EMPTY /'empty'/
C     !beg
      do 100 I = 1,MXI
        HN(I) = EMPTY
        HB(I) = EMPTY
  100 continue
      do 104 I = 1,100
        do 101 J = 1,5
          DM(I,J) = EMPTY
  101   continue
        do 103 K = 1,MXI
          do 102 J = 1,15
            DN(I,J,K) = EMPTY
            DB(I,J,K) = EMPTY
  102     continue
          DN(I,16,K) = EMPTY
  103   continue
  104 continue
C     !end
      return
      end
      subroutine read_mod_stuff
     $(NI,STUFF,BUFF,K,KMX)
C
C     Rudolf Loeser, 2002 Apr 12
C     !DASH
      save
C     !BDECL
      integer NI,K,KMX
      character LINE*80,BUFF*80,STUFF*8
C     !EDECL
C     !DASH
      external buff_it
C
      dimension BUFF(*)
C     !beg
  100 continue
        read (NI,101,end=103) LINE
  101   format(A80)
        call buff_it (LINE,BUFF,K,KMX)
        if(LINE(:8).eq.STUFF) then
          goto 103
        end if
      goto 100
C
  102 continue
      stop 'read_mod_stuff: end-of-stuff marker not found'
C
  103 continue
C     !end
      return
      end
      subroutine buff_it
     $(LINE,BUFF,K,KMX)
C
C     Rudolf Loeser, 2002 Apr 12
C     !DASH
      save
C     !BDECL
      integer K,KMX
      character LINE*80,BUFF*80
C     !EDECL
C     !DASH
      dimension BUFF(*)
C     !beg
      K = K+1
      if(K.gt.KMX) stop 'buff_it: buffer is too short'
      BUFF(K) = LINE
C     !end
      return
      end
      subroutine buff_out
     $(NO,BUFF,K)
C
C     Rudolf Loeser, 2002 Apr 12
C     !DASH
      save
C     !BDECL
      integer NO,K,J,L
      character BUFF*80,LINE*80,EMPTY*5
C     !EDECL
C     !DASH
      dimension BUFF(*)
C
      data EMPTY /'empty'/
C     !beg
      do 102 J = 1,K
        LINE = BUFF(J)
        if(LINE(:10).ne.EMPTY) then
          L = 80
  100     continue
            if(LINE(L-1:L-1).eq.' ') then
              if(L.gt.2) then
                L = L-1
                goto 100
              end if
            end if
          write (NO,101) LINE(:L)
  101     format(A)
        end if
  102 continue
C     !end
      return
      end
      subroutine read_pop_sets
     $(NP,LL)
C
C     Rudolf Loeser, 2002 Apr 12
C     !DASH
      save
C     !BDECL
      real*8 dummy
      integer jummy1,jummy2,jummy3,jummy4,jummy5
      integer NP,KION,LL,J,IONST
      character QELSM*3,qummy1*11,qummy2*8,LINE*80,INAME*3
C     !EDECL
C     !COM
      include 'modup.inc'
C     !DASH
      external RUN_DATA, identify, read_full_N, read_full_B,
     $         read_misc, make_iname
C     !beg
      call RUN_DATA (NP,2,jummy1,jummy2,jummy3,jummy4,jummy5,QELSM,
     $               IONST,qummy1,qummy2,dummy)
      call make_iname (QELSM,IONST,INAME)
      call identify (INAME,KION,ION,MXI)
      if(KION.le.0) stop 'read_pop_sets: unknown pop ion file'
C
  100 format(A80)
      rewind NP
      read (NP,100) LINE
      read (NP,100) LINE
      read (NP,100) LINE
      read (NP,100) LINE
C     !EJECT
      if(KION.eq.1) then
C
        call read_misc (NP,LINE,LL)
        read (NP,100) HN(KION)
        read (NP,100) LINE
        call read_full_N (NP,LINE,1,LL)
        read (NP,100) HB(KION)
        read (NP,100) LINE
        call read_full_B (NP,LINE,1,LL)
        do 101 J = 2,MXI
          read (NP,100) HN(J)
          read (NP,100) LINE
          call read_full_N (NP,LINE,J,LL)
  101   continue
C
      else
C
        read (NP,100) HN(KION)
        read (NP,100) LINE
        if(LINE(:5).ne.LABK(KION)) then
          print 100, HN(KION), LINE
          stop 'read_pop_sets: this does not make sense'
        end if
        call read_full_N (NP,LINE,KION,LL)
        read (NP,100) HB(KION)
        read (NP,100) LINE
        call read_full_B (NP,LINE,KION,LL)
C
      end if
C     !end
      return
      end
      subroutine identify
     $(NAME,INDEX,A,N)
C
C     Rudolf Loeser, 2002 Apr 12
C     !DASH
      save
C     !BDECL
      integer INDEX,N,LOOK
      character NAME*(*),A*(*)
C     !EDECL
C     !DASH
      external LOOKUC
C
      dimension A(*)
C     !beg
      call LOOKUC (A,N,NAME,INDEX,LOOK)
      if(LOOK.eq.2) then
        INDEX = 0
      end if
C     !end
      return
      end
      subroutine make_iname
     $(QELSM,IONST,INAME)
C
C     Rudolf Loeser, 2002 Apr 
C     !DASH
      save
C     !BDECL
      integer IONST,J
      character QELSM*3,INAME*3
C     !EDECL
C     !DASH
C     !beg
      INAME = QELSM
C
      if(IONST.gt.1) then
        if(IONST.lt.10) then
          if(INAME(2:2).eq.' ') then
            J = 2
          else
            J = 3
          end if
          write (INAME(J:J),100) IONST
  100     format(I1)
        else
          print 101, QELSM,IONST
  101     format('[',A,']',I12)
          stop 'make_iname'
        end if
      end if
C     !end
      return
      end
      subroutine read_set
     $(LU,LINE,J,A,L,LMAX)
C
C     Rudolf Loeser, 2002 Apr 12
C
C---- Expects 1. line of 1. data array in LINE
C
C     !DASH
      save
C     !BDECL
      integer LU,J,L,LMAX,JA,K
      character LINE*80,A*80,END*4,LONE*80
C     !EDECL
C     !DASH
      intrinsic index
      dimension A(*)
C
      data END /') > '/
C     !beg
      LONE = LINE
C
      read (LINE,100) JA
  100 format(11X,I3)
      if(J.ne.JA) then
        print 101, J,JA,LINE
  101   format('J =',I5,', JA =',I5/A)
        stop 'read_set: index discrepancy'
      end if
C
      L = 1
      A(L) = LINE
  102 continue
        read (LU,103,end=104) LINE
  103   format(A80)
        L = L+1
        if(L.gt.LMAX) stop 'read_set: too many data lines'
        A(L) = LINE
        K = index(LINE,END)
        if(K.gt.0) goto 105
      goto 102
C
  104 continue
      print 103, LONE
      stop 'read_set: data set lacks proper end'
C
  105 continue
C     !end
      return
      end
      subroutine read_full_N
     $(LU,LINE,KION,LL)
C
C     Rudolf Loeser, 2002 Apr 12
C
C---- Expects 1. line of ionized data array in LINE
C
C     !DASH
      save
C     !BDECL
      integer LU,KION,LL,J,JJ
      character LINE*80
C     !EDECL
C     !COM
      include 'modup.inc'
C     !DASH
      external read_set
C     !beg
C
      J = 0
      call read_set (LU,LINE,J,DN(1,(NL(KION)+1),KION),LL,MXL)
C
      do 103 J = 1,NL(KION)
        read (LU,100) LINE
  100   format(A80)
        if(LINE(:5).ne.LABN(KION)) then
          print 101, J,LABN(KION),LINE
  101     format(' ','J =',I3,' [',A,']'/A)
          stop 'read_full_N'
        end if
C
        call read_set (LU,LINE,J,DN(1,J,KION),JJ,MXL)
C
        if(JJ.ne.LL) then
          print 102, J, LL, JJ
  102     format('J =',I3,', LL =',I5,', JJ =',I5)
          stop 'read_full_N'
        end if
  103 continue
C     !end
      return
      end
      subroutine read_full_B
     $(LU,LINE,KION,LL)
C
C     Rudolf Loeser, 2002 Apr 12
C
C---- Expects 1. line of 1. data array in LINE
C
C     !DASH
      save
C     !BDECL
      integer LU,KION,LL,J,JJ
      character LINE*80
C     !EDECL
C     !COM
      include 'modup.inc'
C     !DASH
      external read_set
C     !beg
      do 103 J = 1,NL(KION)
        if(J.ne.1) then
          read (LU,100) LINE
  100     format(A80)
          if(LINE(:5).ne.LABB(KION)) then
            print 101, J,LABB(KION),LINE
  101       format('J +',I3,' [',A,']'/A)
            stop 'read_full_B'
          end if
        end if
C
        call read_set (LU,LINE,J,DB(1,J,KION),JJ,MXL)
C
        if(JJ.ne.LL) then
          print 102, J, LL, JJ
  102     format('J =',I3,', LL =',I5,', JJ =',I5)
          stop 'read_full_B'
        end if
  103 continue
C     !end
      return
      end
      subroutine read_misc
     $(LU,LINE,LL)
C
C     Rudolf Loeser, 2002 Apr 12
C     !DASH
      save
C     !BDECL
      integer LU,LL,J,JJ
      character LINE*80
C     !EDECL
C     !COM
      include 'modup.inc'
C     !DASH
      external read_set
C     !beg
      do 102 J = 1,5
        read (LU,100) LINE
  100   format(A80)
        read (LU,100) LINE
        if(LINE(:5).ne.LABM(J)) then
          print 101, J,LABM(J),LINE
  101     format('J =',I2,' [',A,']'/A)
          stop 'read_misc'
        end if
        call read_set (LU,LINE,0,DM(1,J),JJ,MXL)
  102 continue
C     !end
      return
      end
      subroutine read_mod_sets
     $(NI,LL)
C
C     Rudolf Loeser, 2002 Apr 12
C     !DASH
      save
C     !BDECL
      integer NI,LL,JJ,KION
      character LINE*80,LANE*80,END*16
C     !EDECL
C     !COM
      include 'modup.inc'
C     !DASH
      external read_misc, identify, read_full_N, read_full_B
C
      data END /'USE ( INPUT ) > '/
C     !beg
C
      call read_misc (NI,LINE,LL)
C
      read (NI,100) LINE
  100 format(A80)
      if(LINE(:16).ne.END) then
        print 101, END,LINE
  101   format('[',A,'] expected but not found'/A)
        stop 'read_mod_sets'
      end if
C     !EJECT
  102 continue
        read (NI,100) LANE
        if(LANE(:16).eq.END) goto 103
C
        read (NI,100) LINE
C
        call identify (LINE(:5),KION,LABK,MXI)
        if(KION.gt.0) then
          HN(KION) = LANE
          call read_full_N (NI,LINE,KION,JJ)
          goto 102
        end if
C
        call identify (LINE(:5),KION,LABB,MXI)
        if(KION.gt.0) then
          HB(KION) = LANE
          call read_full_B (NI,LINE,KION,JJ)
          goto 102
        end if
C
      print 100, LANE,LINE
      stop 'read_mod_sets'
C
  103 continue
C     !end
      return
      end
