      program ready
C
C     Rudolf Loeser, 2002 Apr 10
C          corrected 2006 Aug 29
C                    2006 Nov 17
C                    2007 Mar 19
C                    2007 May 02
C
C---- Readies a PANDORA input file (---.dat) by modifying
C     the header and the IOMX statement
C     (and RABDs and diffusion stuff as needed).
C     !BDECL
      integer NI,NO,NR,NP,IGO,IT,J,K,KMX,L
      character LINE*80,BUFF*80
      logical HAVER,DIDR,HAVEP,DIDP,JGO
C     !EDECL
C     !DASH
      external  modify_header, use_rabd, use_palbet, buff_it,
     $          actual_input
      intrinsic index
C
      dimension BUFF(10000)
C
      data KMX /10000/
      data NI,NO,NR,NP /61, 62, 63, 64/
C     !beg
      call actual_input (NR,HAVER)
      DIDR = .false.
      call actual_input (NP,HAVEP)
      DIDP = .false.
C
      IGO = 0
C
      print 100
  100 format('Print desired value of IOMX (I2): ',$)
      accept 101, IT
  101 format(I2)
C
      K = 0
C
      rewind NI
      read (NI,102) LINE
  102 format(A80)
      call modify_header (LINE(:65),IT)
      print 202, LINE
  202 format(' Updated header: ',A)
      call buff_it (LINE,BUFF,K,KMX)
C     !EJECT
  103 continue
C
        read (NI,102) LINE
C
        JGO = LINE(:2).eq.'GO'
        if(JGO) then
          IGO = IGO+1
        end if
        if(IGO.eq.1) then
C
          J = index(LINE(:50),'IOMX')
          if(J.gt.0) then
            write (LINE,104) IT
  104       format('IOMX ( ',I2,' ) > ')
            print 102, LINE
          end if
C
          J = index(LINE(:50),'RABD')
          if(J.gt.0) then
            call use_rabd (NI,LINE,NR,BUFF,K,KMX,HAVER,DIDR,1)
          end if
C
          J = index(LINE(:50),'PALBET')
          if(J.gt.0) then
            call use_palbet (NI,LINE,NP,BUFF,K,KMX,HAVEP,DIDP,1)
          end if
C
        end if
C
C >>>   This is the "m a s t e r" buffit, and it expects use_rabd
C       and use_palbet to behave appropriately
C
        call buff_it (LINE,BUFF,K,KMX)
C 
        if(JGO.and.(IGO.eq.2)) then
          if(.not.DIDR) then
C           Ensure the "GO" will be overwritten
C           (use-rabd will provide a new one)
            K = K-1
C           Make sure to use the available RABD
            call use_rabd (NI,LINE,NR,BUFF,K,KMX,HAVER,DIDR,2)
          end if
          if(.not.DIDP) then
C           Ensure the "GO" will be overwritten
C           (use-palbet will provide a new one)
            K = K-1
C           Make sure to use the available RABD
            call use_palbet (NI,LINE,NP,BUFF,K,KMX,HAVEP,DIDP,2)
          end if
        end if
C
      if(IGO.lt.4) goto 103
C     !EJECT
      rewind NO
      do 107 J = 1,K
        L = 80
  105   continue
          if(BUFF(J)(L:L).eq.' ') then
            if(L.gt.2) then
              L = L-1
              goto 105
            end if
          end if
        write (NO,106) BUFF(J)(:L)
  106   format(A)
  107 continue
C
      print 108, K,KMX
  108 format(' ','Ready 4.00:  # of lines =',I5,' (limit =',I6,')')
C
      stop 'ready 4.00: done'
C     !end
      end
      subroutine actual_input
     $(LU,YES)
C
C     Rudolf Loeser, 2007 May 02
C     !DASH
      save
C     !BDECL
      integer LU
      character LINE*80
      logical YES
C     !EDECL
C     !DASH
C     !beg
      YES = .false.
      rewind LU
      read (LU,100,end=101) LINE
  100 format(A80)
      YES = .true.
  101 continue
C     !end
      return
      end
      subroutine buff_it
     $(LINE,BUFF,K,KMX)
C
C     Rudolf Loeser, 2002 Apr 11
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
      if(K.gt.KMX) stop 'buff-it: buffer is too short'
      BUFF(K) = LINE
C     !end
      return
      end
      subroutine has_end
     $(LINE,YES)
C
C     Rudolf Loeser, 2006 Aug 29
C     !DASH
      save
C     !BDECL
      integer K
      character LINE*(*),END*3
      logical YES
C     !EDECL
C     !DASH
      intrinsic index
C
      data END /') >'/
C     !beg
      K   = index(LINE,END)
      YES = K.gt.0
C     !end
      return
      end
      subroutine modify_header
     $(HEAD,JT)
C
C     Rudolf Loeser, 2002 Apr 10
C     !DASH
      save
C     !BDECL
      integer IT,JT
      character HEAD*65
C     !EDECL
C     !DASH
      external  GET_DATE
      intrinsic mod
C     !beg
      if(HEAD(45:45).eq.' ') then
        HEAD(45:45) = '0'
      end if
      read (HEAD(45:46),100) IT
  100 format(I2)
      if(JT.eq.1) then
        HEAD(47:52) = '      '
      else
        IT = mod((IT+JT-1),100)
        write (HEAD(48:52),101) IT
  101   format(I2,'   ')
        if(HEAD(48:48).eq.' ') then
          HEAD(48:48) = '0'
        end if
      end if
      call GET_DATE (HEAD(53:63))
C     !end
      return
      end
      subroutine use_rabd
     $(NI,LINE,NR,BUFF,K,KMX,HAVER,DIDR,KALL)
C
C     Rudolf Loeser, 2002 Apr 11
C
C
C                        REMEMBER 
C                        ========
C
C     to leave last line of data in LINE when KALL = 1,
C     to buff_it LINE = GO when KALL = 2.
C
C
C     !DASH
      save
C     !BDECL
      integer NI,NR,K,KMX,KALL
      character LINE*80,BUFF*80
      logical YES,HAVER,DIDR
C     !EDECL
C     !DASH
      external buff_it, has_end
C
      dimension BUFF(*)
C     !beg
      if((KALL.lt.1).or.(KALL.gt.2)) stop 'use_rabd: KALL'
C
      if(KALL.eq.1) then
C----   KALL = 1: RABDs found in old .dat
        if(HAVER) then
          print 200
  200     format(' ','Updating RABD')
C----     Read and USE every replacement RABD line from NR
          rewind NR
  100     continue
            read (NR,101,end=103) LINE
  101       format(A80)
            call has_end (LINE,YES)
            if(YES) then
              DIDR = .true.
C             (Don't buff_it the last LINE)
              goto 102
            end if
            call buff_it (LINE,BUFF,K,KMX)
          goto 100
C
  102     continue
C----       SKIP all old RABD lines from NI
            read (NI,101) LINE
            call has_end (LINE,YES)
            if(YES) then
C             All done
              goto 106
            end if
          goto 102
C
  103     continue
C           End-of-file occurred before end of data
            stop 'use_rabd: file .rab lacks a proper end'
C
        else
          print 201
  201     format(' ','No new RABD available - use the old one')
C         NR is empty - read and use ALL old RABD lines from NI
          call buff_it (LINE,BUFF,K,KMX)
  104     continue
            read (NI,101) LINE
            call has_end (LINE,YES)
            if(YES) then
              DIDR = .true.
C             (Don't buff_it the last LINE)
              goto 106
            end if
            call buff_it (LINE,BUFF,K,KMX)
          goto 104
        end if
C
      else
C----   KALL = 2: no RABDs in old .dat
        if(HAVER) then
          print 202
  202     format(' ','New RABD being added to new .dat file')
          rewind NR
  105     continue
C----       Read and USE every RABD line from NR
            read (NR,101,end=103) LINE
            call has_end (LINE,YES)
            call buff_it (LINE,BUFF,K,KMX)
            if(YES) then
              DIDR = .true.
              LINE = 'GO > '
              call buff_it (LINE,BUFF,K,KMX)
              goto 106
            end if
          goto 105
        else
          print 203
  203     format(' ','No RABD available for new .dat file ',
     $               '(but that may be OK---think about it)')
          LINE = 'GO > ' 
          call buff_it (LINE,BUFF,K,KMX)
        end if
C
      end if
C
  106 continue
C     !end
      return
      end
      subroutine use_palbet
     $(NI,LINE,NP,BUFF,K,KMX,HAVEP,DIDP,KALL)
C
C     Rudolf Loeser, 2002 Apr 11
C
C
C                        REMEMBER 
C                        ========
C
C     to leave last line of data in LINE when KALL = 1,
C     to buff_it LINE = GO when KALL = 2.
C
C
C     !DASH
      save
C     !BDECL
      integer NI,NP,K,KMX,KP,KE,KALL
      character LINE*80,BUFF*80
      logical YES,HAVEP,DIDP
C     !EDECL
C     !DASH
      external buff_it, has_end
C
      dimension BUFF(*)
C     !beg
      if((KALL.lt.1).or.(KALL.gt.2)) stop 'use_palbet: KALL'
C
      if(KALL.eq.1) then
C----   KALL = 1: PALBET found in old .dat
        if(HAVEP) then
          print 200
  200     format(' ','Updating PALBET')
C
          if(BUFF(K)(:2).eq.'> ') then
C           Make sure that the old run header preceding the
C           PALBET line (which has already been saved by the
C           "master" buff_it in the main program) is
C           overwritten
            K = K-1
          end if
C----     Read and USE every replacement PALBET line from NP
          rewind NP
          KP = 0
          KE = 0
  100     continue
            read(NP,101,end=104) LINE
  101       format(A80)
            KP = KP+1
            call has_end (LINE,YES)
            if(YES) then
              KE = KE+1
              if(KE.eq.2) then
C               Stop only after 2 ends have been found
                DIDP = .true.
C               (Don't buff_it the last LINE)
                goto 102
              end if
            end if
            call buff_it (LINE,BUFF,K,KMX)
          goto 100
C
  102     continue
C----     SKIP all old PALBET lines from NI
          KE = 0
  103     continue
            read (NI,101) LINE
            call has_end (LINE,YES)
            if(YES) then
              KE = KE+1
              if(KE.eq.2) then
C               Stop only after 2 ends have been found
                goto 107
              end if
            end if
          goto 103
C 
  104     continue
C----       End-of-file occurred before end of data
            stop 'use palbet: file NP lacks a proper end'
C
        else
          print 201
  201     format(' ','No new PALBET available - use the old one')
C----     NP is empty --- read and use ALL old PALBET lines from NI
          call buff_it (LINE,BUFF,K,KMX)
          KE = 0
  105     continue
            read (NI,101) LINE
            call has_end (LINE,YES)
            if(YES) then
              KE = KE+1
              if(KE.eq.2) then
C               Stop only after 2 ends have been found
                DIDP = .true.
C               (Don't buff_it the last LINE)
                goto 107
              end if
            end if
            call buff_it (LINE,BUFF,K,KMX)
          goto 105
        end if
C
      else
C----   KALL = 2: no PALBET in old .dat
        if(HAVEP) then
          print 202
  202     format(' ','New PALBET being added to new .dat file')
          rewind NP
          KP = 0
          KE = 0
  106     continue
C----       Read and USE every PALBET line from NP
            read(NP,101,end=104) LINE
            call buff_it (LINE,BUFF,K,KMX)
            KP = KP+1
            call has_end (LINE,YES)
            if(YES) then
              KE = KE+1
              if(KE.eq.2) then
C               Stop only after 2 ends have been found
                DIDP = .true.
                LINE = 'GO > '
                call buff_it (LINE,BUFF,K,KMX)
                goto 107
              end if
            end if
C           call buff_it (LINE,BUFF,K,KMX)
          goto 106
        else
          print 203
  203     format(' ','No PALBET available for new .dat file ',
     $               '(but that may be OK---think about it)')
          LINE = 'GO > ' 
          call buff_it (LINE,BUFF,K,KMX)
        end if
C
      end if
C
  107 continue
C     !end
      return
      end
