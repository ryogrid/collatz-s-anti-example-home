[form1.frm]

Option Explicit
Dim mintPortNumber As Integer
Dim mintSeverNumber As Integer
Dim mobjSever(0 To 500) As severclass
Dim mintWithin As Long
Dim mintWaitPort(1000)
Dim muPreserved(50) As Preserved         '�N���C�A���g�̃f�[�^��ێ�����z��
Dim mintArrangeNumber As Integer          'muPreserved�̔z��̎��Ɏg�p���ׂ��Y����
Dim mintNowRange As Integer               '���ɂǂ̒l��^����K�v�����邩
Dim strDiscoveredPrime As String
Dim mConfirmedLimit As Integer             '���肳�ꂽ����ێ����Ă����t�@�C���̃��R�[�h���i���Ɏg�p���ׂ����j
Dim lngPreparePrime(77269, 3) As Long             '�N���C�A���g�ւ̔z���p�w���ł���f�� �Q�����ڂɂ́A���ۂ̑f���ƁA����U������,�f���ł��邩�ǂ�����ێ���true=1 falase=2

Private Sub Command1_Click()    '�I������
    Unload Me
End Sub

Private Sub Command2_Click()
Dim intFilenumber As Integer
Dim strexist As String
Dim intReturn As Integer
Dim work As Confirmed
     
    intReturn = MsgBox("�{���ɂ�낵���ł����H", vbOKCancel + vbCritical)
    If intReturn = vbOK Then
        Call FileCodeOut("ClientData", xORnumber)
        strexist = Dir(App.Path & "\" & "ClientData")            'ClientData���N���A����
        If strexist = "ClientData" Then
            Kill App.Path & "\" & "ClientData"
            intFilenumber = FreeFile
            Open App.Path & "\" & "ClientData" For Random As intFilenumber Len = Len(muPreserved(0))         '�v�Z���I������͈͂���������
            Close #intFilenumber
            Call FileCodeIn("ClientData", xORnumber)      '�t�@�C�����Í���
        End If
        Call FileCodeOut("ConfirmedNumber", xORnumber)
        strexist = Dir(App.Path & "\" & "ConfirmedNumber")            'ComfirmedNumber���N���A����
        If strexist = "ConfirmedNumber" Then
            Kill App.Path & "\" & "ConfirmedNumber"
            intFilenumber = FreeFile
            Open App.Path & "\" & "ConfirmedNumber" For Random As intFilenumber Len = Len(work)         '�v�Z���I������͈͂���������
            Close #intFilenumber
            Call FileCodeIn("ConfirmedNumber", xORnumber)            '�t�@�C�����Í���
        End If
        MSFlexGrid1.Rows = 1                                         'MSflexGrid�R���g���[�����N���A�[����
        MSFlexGrid1.Cols = 4
        intReturn = MsgBox("�X�V���ʂ𔽉f���邽�߈�x�I�����܂�", vbOKOnly, "�I��")
        End
    End If
End Sub

Private Sub Form_Load()
Dim intFilenumber As Integer
Dim n As Long
        
        Form1.Caption = "Collatz's Anti-Exp���������� Server" & "   #�A�h���X:" & Winsock1.LocalIP
        StatusBar1.SimpleText = "�ڑ��ҋ@��"
        MSFlexGrid1.Rows = 1                                         'MSflexGrid�R���g���[���̏����ݒ�
        MSFlexGrid1.Cols = 11
        MSFlexGrid1.ColWidth(-1) = MSFlexGrid1.Width / 4
        MSFlexGrid1.RowHeight(-1) = MSFlexGrid1.Height / 20
        MSFlexGrid1.TextMatrix(0, 0) = "���[�U�[��"
        MSFlexGrid1.TextMatrix(0, 1) = "���[�U�[�ԍ�"
        MSFlexGrid1.TextMatrix(0, 2) = "�h�o�A�h���X"
        MSFlexGrid1.TextMatrix(0, 3) = "�O��v�Z������"
        MSFlexGrid1.TextMatrix(0, 4) = "�v�Z���̐�"
        MSFlexGrid1.TextMatrix(0, 5) = "�Ԃ������[�N��"
        MSFlexGrid1.TextMatrix(0, 6) = "�v���O�������N������"
        MSFlexGrid1.TextMatrix(0, 7) = "�b�o�t�̃N���b�N��"
        MSFlexGrid1.TextMatrix(0, 8) = "�b�o�t�̎��"
        MSFlexGrid1.TextMatrix(0, 9) = "������������"
        MSFlexGrid1.TextMatrix(0, 10) = "���b�Z�[�W"
        
        MSFlexGrid1.ColAlignment(-1) = flexAlignCenterCenter         '�Z���̓��e���㉺�Ƃ��ɒ��������ŕ\������
        
        mintSeverNumber = 1
        mintPortNumber = 1002
        Winsock1.LocalPort = 1001   '�ڑ��v����t�|�[�g�ԍ��ݒ�
        Winsock1.Listen   '�ڑ��v���҂�
        Clipboard.SetText 5
        n = 1
        Call FileCodeOut("ClientData", xORnumber)            '�t�@�C���𕜍���
        intFilenumber = FreeFile
        Open App.Path & "\" & "ClientData" For Random As intFilenumber Len = Len(muPreserved(0))         '�v�Z���I������͈͂���������
            Do While Not EOF(intFilenumber)
                Get #intFilenumber, n, muPreserved(n)        '���W�X�g���[�t�@�C����������擾
                  If muPreserved(n).intFileAddres <> 0 Then  '�t�@�C���Ƀf�[�^���܂܂�Ă���ꍇ
                      Call AddList(muPreserved(n))
                      If Left(muPreserved(n).strCulculated, 4) = "���ݏ���" Then
                           MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 3) = "���ݏ���"
                      End If
                      If Left(muPreserved(n).strCpuType, 6) = "secret" Then   '�N���C�A���g���X�y�b�N���̌��J�������Ă��Ȃ��ꍇ
                            MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 7) = "secret"
                            MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 9) = "secret"
                      End If
                  End If
                n = n + 1
            Loop
        Close #intFilenumber
        Call FileCodeIn("ClientData", xORnumber)            '�t�@�C�����Í���
        mintNowRange = 1
        If muPreserved(1).intNowRange <> 0 Then           '���łɌv�Z���ꂽ�͈͂�����ꍇ
                      mintNowRange = muPreserved(1).intNowRange
        End If
        strDiscoveredPrime = muPreserved(1).strDiscoveredPrime
        lngPreparePrime(mintNowRange, 1) = muPreserved(1).intDistributeCount
        mintArrangeNumber = n - 1
        intFilenumber = FreeFile                     '�o�C�i���ŕێ����Ă���f����z��ɓǂݍ���
        Open "PriparePRIME" For Random As intFilenumber Len = Len(lngPreparePrime(1, 0))
            For n = 1 To 501
                  Get #intFilenumber, n, lngPreparePrime(n, 0)
            Next n
        Close #intFilenumber
        mConfirmedLimit = 1
        Call LoadConfirmed              '���݂܂ł̔��茋�ʂ�z��ɑ������
        Call ShowConfirmed               '���茋�ʂ�\������
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
        Call DataSave
End Sub

Private Sub Winsock1_Close()
        Winsock1.Close
        Winsock1.Listen
End Sub

Private Sub Winsock1_ConnectionRequest(ByVal requestid As Long)
        Winsock1.Close
        Winsock1.Accept requestid
End Sub

Private Sub Winsock1_DataArrival(ByVal bytesTotal As Long)
Dim i As Integer
Dim n As Integer
Dim strDat As String   'strDat=��M�f�[�^
Static stcstrDat As String
Dim valsplited As Variant
Dim intClientNumber As Integer

         Winsock1.GetData strDat    '�N���C�A���g����f�[�^����M
         stcstrDat = stcstrDat & strDat
         If Right(strDat, 1) = "#" Then
                stcstrDat = Left(stcstrDat, Len(stcstrDat) - 1) '�E�[��#����菜��
                stcstrDat = OutCode(stcstrDat, xORnumber)     '�f�[�^�𕜍�������
                valsplited = Split(stcstrDat, "!")                 '�I����؂蕶���Ƃ��đ��M����Ă����f�[�^�𕪊�����
                intClientNumber = Val(valsplited(2))
ErrorBack: '���N���C�A���g�̕ێ����Ă����񂪈�v���Ȃ��ꍇ���߂Ă̐ڑ��Ƃ��ăX�^�[�g����
                If intClientNumber = 0 Then                     '�N���C�A���g���n�߂Ă̐ڑ��̏ꍇ�O�������Ă���̂ł��̏ꍇ
                    muPreserved(mintArrangeNumber).strName = SpaceTrim(CStr(valsplited(0)))
                    StatusBar1.SimpleText = muPreserved(mintArrangeNumber).strName & "����̐ڑ�������"
                    muPreserved(mintArrangeNumber).strAddres = Winsock1.RemoteHostIP
                    muPreserved(mintArrangeNumber).strCulculated = "���ݏ���"
                    muPreserved(mintArrangeNumber).intFileAddres = mintArrangeNumber
                    muPreserved(mintArrangeNumber).lngNowCulculate = lngPreparePrime(mintNowRange, 0)
                    muPreserved(intClientNumber).lngNowCulculateArray = mintNowRange    '�v�Z���Ă���w���̕ۑ�����Ă���ꏊ���ێ����Ă���
                    lngPreparePrime(mintNowRange, 1) = lngPreparePrime(mintNowRange, 1) + 1        '����U�����񐔂Ƃ��ĂP���Z
                    If lngPreparePrime(mintNowRange, 1) >= 2 Then            '���łɂQ�񊄂�U�����ꍇ�A���̒l������U�邽�߂P���Z���Ă���
                                mintNowRange = mintNowRange + 1
                    End If
                    muPreserved(mintArrangeNumber).lngWorkCounts = 1
                    muPreserved(mintArrangeNumber).strMassageFrom = valsplited(4)
                    muPreserved(mintArrangeNumber).intCpuClock = CInt(valsplited(5))
                    muPreserved(mintArrangeNumber).strCpuType = valsplited(6)
                    muPreserved(mintArrangeNumber).intMemorySize = CInt(valsplited(7))
                    muPreserved(mintArrangeNumber).lngClientWorkingTime = CLng(valsplited(8))
                    n = 1                                            '���݃��[�N���ŏ��ʂ������Ȃ̂����ׂ�
                    For i = 1 To mintArrangeNumber - 1
                          If muPreserved(mintArrangeNumber).lngWorkCounts > 1 Then        '�ڑ����̃N���C�A���g��胏�[�N���̑����N���C�A���g�������ꍇ
                                      n = n + 1
                          End If
                    Next i
                    muPreserved(mintArrangeNumber).intPrize = n
                    intClientNumber = mintArrangeNumber
                    Call AddList(muPreserved(mintArrangeNumber))
                    MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 3) = "���ݏ���"
                    If valsplited(6) = "secret" Then    '�N���C�A���g���X�y�b�N���̌��J�������Ă��Ȃ��ꍇ
                            MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 7) = "secret"
                            MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 9) = "secret"
                    End If
                    mintArrangeNumber = mintArrangeNumber + 1
                Else
                    If muPreserved(intClientNumber).lngNowCulculate = CLng(valsplited(1)) Then     '�N���C�A���g�̏�񂪕ێ����Ă�����ƈ�v���Ă����ꍇ
                        StatusBar1.SimpleText = muPreserved(intClientNumber).strName & "����̐ڑ�������"
                        muPreserved(intClientNumber).strCulculated = valsplited(1)
                        muPreserved(intClientNumber).lngWorkCounts = muPreserved(intClientNumber).lngWorkCounts + 1
                        muPreserved(intClientNumber).lngNowCulculate = lngPreparePrime(mintNowRange, 0)
                        muPreserved(intClientNumber).lngNowCulculateArray = mintNowRange    '�v�Z���Ă���w���̕ۑ�����Ă���ꏊ���ێ����Ă���
                        muPreserved(intClientNumber).lngClientWorkingTime = CLng(valsplited(8))
                        lngPreparePrime(mintNowRange, 1) = lngPreparePrime(mintNowRange, 1) + 1        '����U�����񐔂Ƃ��ĂP���Z
                        If lngPreparePrime(mintNowRange, 1) >= 2 Then            '���łɂQ�񊄂�U�����ꍇ�A���̒l������U�邽�߂P���Z���Ă���
                                mintNowRange = mintNowRange + 1
                        End If
                        n = 1                                            '���݃��[�N���ŏ��ʂ������Ȃ̂����ׂ�
                        For i = 1 To mintArrangeNumber - 1
                          If muPreserved(mintArrangeNumber).lngWorkCounts > 1 Then        '�ڑ����̃N���C�A���g��胏�[�N���̑����N���C�A���g�������ꍇ
                                      n = n + 1
                          End If
                        Next i
                        muPreserved(mintArrangeNumber).intPrize = n
                        Call RenewList(muPreserved(intClientNumber), intClientNumber)
                        If valsplited(3) = "1" Then                      '�N���C�A���g���f���𔭌����Ă����ꍇ
                                strDiscoveredPrime = SpaceTrim(strDiscoveredPrime) & "," & SpaceTrim(muPreserved(intClientNumber).strCulculated)
                                lngPreparePrime(muPreserved(intClientNumber).lngNowCulculateArray, 2) = 1        '�z��ɑf���ł��鎖���L�^���Ă���
                                Call SaveConfirmed(muPreserved(intClientNumber).lngNowCulculateArray, 1)         '�t�@�C���ɋL�^���Ă���
                                MSFlexGrid1.TextMatrix(intClientNumber, 3) = "�Q�O" & CLng(muPreserved(intClientNumber).strCulculated) & "�|�P is prime!!"
                        Else      '�f���ł͂Ȃ������ꍇ
                                lngPreparePrime(muPreserved(intClientNumber).lngNowCulculateArray, 2) = 2        '�z��ɑf���ł��鎖���L�^���Ă���
                                Call SaveConfirmed(muPreserved(intClientNumber).lngNowCulculateArray, 2)          '�t�@�C���ɋL�^���Ă���
                        End If
                        Call ShowConfirmed                '�z��̓��e�����X�g�{�b�N�X�ɕ\������
                    Else     '�N���C�A���g�̏�񂪕ێ����Ă�����ƈ�v���Ȃ��ꍇ
                          StatusBar1.SimpleText = "�N���C�A���g���T�[�o�[�̕ێ����Ă�����Ɍ�肪����\��������܂�"
                          intClientNumber = 0                                       '�܂�A�T�[�o�̕ێ����Ă����񂪃��Z�b�g���ꂽ���A�N���C�A���g�̏��Ɉُ킪����ꍇ
                          GoTo ErrorBack           '�ŏ��̐ڑ��Ƃ��Ă�蒼���̂ŏ�փW�����v����
                    End If
                End If
               
                   Winsock1.SendData InCode("##" & "!" & intClientNumber & "!" & muPreserved(intClientNumber).lngNowCulculate & "!" & muPreserved(intClientNumber).lngWorkCounts & "!" & muPreserved(intClientNumber).intPrize, xORnumber) & "#"       '�Í������đ��M����
                    stcstrDat = ""       '�ÓI�ȕϐ����Ăу��Z�b�g����
                   StatusBar1.SimpleText = "�ڑ��ҋ@��"
         End If
End Sub

Private Sub Winsock1_SendComplete()
        Winsock1.Close
        Winsock1.Listen
End Sub

Private Function AddList(Data As Preserved)                 '�\���̂������ɓn���A�O���b�h�R���g���[���Ƀf�[�^��ǉ�����
     MSFlexGrid1.AddItem SpaceTrim(Data.strName)
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 1) = Data.intFileAddres
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 2) = RTrim(Data.strAddres)
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 3) = "�Q�O" & SpaceTrim(Data.strCulculated) & "�|�P"
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 4) = "�Q^" & Data.lngNowCulculate & "�|�P"
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 5) = Data.lngWorkCounts
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 6) = ChangeTime(Data.lngClientWorkingTime)
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 7) = Data.intCpuClock & "Mhz"
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 8) = SpaceTrim(Data.strCpuType)
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 9) = Data.intMemorySize & "M"
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 10) = SpaceTrim(Data.strMassageFrom)
End Function

Private Function RenewList(Data As Preserved, FileNumber As Integer)     '�\���̂������ɓn���A�O���b�h�R���g���[���Ƀf�[�^��ǉ�����(���łɏ������܂�Ă��镔��������������j
     MSFlexGrid1.TextMatrix(FileNumber, 3) = "�Q�O" & SpaceTrim(Data.strCulculated) & "�|�P"
     MSFlexGrid1.TextMatrix(FileNumber, 4) = "�Q�O" & Data.lngNowCulculate & "�|�P"
     MSFlexGrid1.TextMatrix(FileNumber, 5) = Data.lngWorkCounts
     MSFlexGrid1.TextMatrix(FileNumber, 6) = ChangeTime(Data.lngClientWorkingTime)
End Function

Private Function DataSave()                              '�N���C�A���g�̏���ۑ�����֐�
Dim intFilenumber As Integer
Dim n As Integer
Dim i As Integer
    
    intFilenumber = FreeFile
    muPreserved(1).intNowRange = mintNowRange     '��Ԗڂ̃��R�[�h�����Ɍ��ݒT�����I����Ă���͈͂���������ł���
    muPreserved(1).intDistributeCount = lngPreparePrime(mintNowRange - 1, 1)     '���x����U���������ۑ����Ă���
    muPreserved(1).strDiscoveredPrime = strDiscoveredPrime
    i = 1
    Call FileCodeOut("ClientData", xORnumber)            '�t�@�C���𕜍���
    Open App.Path & "\" & "ClientData" For Random As intFilenumber Len = Len(muPreserved(1))         '���̎��_�ł̏�����������
       For n = 1 To mintArrangeNumber - 1               '���ݕێ����Ă���N���C�A���g�̏����t�@�C���ɏ�������
          If muPreserved(n).intFileAddres <> 0 Then
              Put #intFilenumber, i, muPreserved(n)
          End If
          i = i + 1
       Next n
    Close #intFilenumber
    Call FileCodeIn("ClientData", xORnumber)            '�t�@�C���𕜍���
    
End Function

Private Function SpaceTrim(Data As String) As String        '�����_���A�N�Z�X�ŏ������񂾎��ɖ����ɖ��߂�ꂽ�X�y�[�X����菜���֐�
    SpaceTrim = Trim(Replace(Data, Chr(0), ""))
End Function

Private Sub MSFlexGrid1_MouseMove(Button As Integer, _
                    Shift As Integer, x As Single, y As Single)
       If mintArrangeNumber > 1 Then      '���łɃN���C�A���g����P��ȏ�ڑ����������ꍇ
          With MSFlexGrid1
                 MSFlexGrid1.ToolTipText = .TextMatrix(0, .MouseCol) & "�F" & .TextMatrix(.MouseRow, .MouseCol)
          End With
       End If
End Sub

Private Function SaveConfirmed(one As Long, two As Integer)           '�m�F���ꂽ����ۑ����Ă����֐��A�����P=pripareprime�̂P�����ځ@�����Q=1:true or 2:false
Dim Number As Confirmed
Dim intFilenumber As Integer
                  
                  Call FileCodeOut("ConfirmedNumber", xORnumber)            '�t�@�C���𕜍���
                  Number.lngPrimeNumber = one
                  Number.intTrueOrFalse = two
                  intFilenumber = FreeFile
                  Open App.Path & "\" & "ConfirmedNumber" For Random As intFilenumber Len = Len(Number)
                          Put #intFilenumber, mConfirmedLimit, Number
                  Close #intFilenumber
                  Call FileCodeIn("ConfirmedNumber", xORnumber)            '�t�@�C�����Í���
                  mConfirmedLimit = mConfirmedLimit + 1
End Function

Private Function LoadConfirmed()                                          '�m�F���ꂽ����ۑ����Ă������t�@�C��ConfirmedNumber����ǂݍ���
Dim Number As Confirmed                                                    '�����R�[�h�̂P�Ԃɑ����R�[�h����ێ����Ă���
Dim intFilenumber As Integer
Dim work As Integer
Dim strexist As String
                  
On Error Resume Next
                  Call FileCodeOut("ConfirmedNumber", xORnumber)            '�t�@�C���𕜍���
                  strexist = Dir(App.Path & "\" & "ConfirmedNumber")
                  If strexist = "ConfirmedNumber" Then
                        Call FileCodeOut("ConfirmedNumber", xORnumber)            '�t�@�C���𕜍���
                        intFilenumber = FreeFile
                        Open App.Path & "\" & "ConfirmedNumber" For Random As intFilenumber Len = Len(Number)
                        Do While Not EOF(intFilenumber)
                                Get #intFilenumber, work + 1, Number
                                lngPreparePrime(Number.lngPrimeNumber, 2) = Number.intTrueOrFalse
                                work = work + 1
                        Loop
                        Close #intFilenumber
                        Call FileCodeIn("ConfirmedNumber", xORnumber)            '�t�@�C�����Í���
                        mConfirmedLimit = work + 1         '���Ɏg�p���ׂ����R�[�h�ԍ���ێ����Ă���
                  End If
End Function

Private Function ChangeTime(ByVal Second As Long) As String             '�b����n���ƁA�����ԁA�����A���b�ɒ����āA������Ƃ��ĕԂ��֐�
Dim Hour As Long
Dim Minute As Long
Dim work As Long
Dim str As String

            Hour = Second \ 3600
            work = Second Mod 3600
            Minute = work \ 60
            Second = work Mod 60
            If Hour <> 0 Then
                  str = Hour & "����"
            End If
            If Minute <> 0 Then
                  str = str & Minute & "��"
            End If
            If Second <> 0 Then
                  str = str & Second & "�b"
            End If
            ChangeTime = str
End Function

Private Function ShowConfirmed()             '���X�g�{�b�N�X�R���g���[���ɔz��ɑ�����Ă��锻�茋�ʂ����ׂĕ\������
Dim i As Integer

                 List1.Clear
                 For i = 1 To mintNowRange - 1
                       Select Case lngPreparePrime(i, 2)
                             Case 0
                                 List1.AddItem "2^" & lngPreparePrime(i, 0) & "-1 is Now Culculate"
                             Case 1
                                 List1.AddItem "2^" & lngPreparePrime(i, 0) & "-1 is Anti-Example?"
                             Case 2
                                 List1.AddItem "2^" & lngPreparePrime(i, 0) & "-1 is OK"
                             End Select
                 Next i
End Function

Private Function InCode(ByVal str As String, xORnumber As Integer) As String           '�������n���ƈÍ������ꂽ�������Ԃ��i�w���������Ō�������l���K�v�j
Dim bytArray() As Byte
Dim i As Long
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer

            xORnumber2 = xORnumber \ 2 + 1        '�Q�x�ڂ�Xor�����ŗ��p����l
            xORnumber3 = xORnumber \ 3 + 2
            ReDim bytArray(lstrlen(str))
            Call MoveMemory(bytArray(0), ByVal str, lstrlen(str))
            str = ""
            For i = 0 To UBound(bytArray) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber)
            Next i
            For i = 1 To UBound(bytArray) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber2)
            Next i
            For i = 0 To UBound(bytArray)
                bytArray(i) = CByte(bytArray(i) Xor xORnumber3)
                str = str & "," & bytArray(i)
            Next i
                str = Right(str, Len(str) - 1)
                InCode = str
End Function

Private Function OutCode(ByVal str As String, xORnumber As Integer)                 '�Í������ꂽ�������n���ƕ��������ĕԂ��i�w���������Ō��������l���K�v�j
Dim bytArray() As Byte
Dim valsplited As Variant
Dim i As Long
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer

            xORnumber2 = xORnumber \ 2 + 1        '�Q�x�ڂ�Xor�����ŗ��p����l
            xORnumber3 = xORnumber \ 3 + 2
            valsplited = Split(str, ",")
            ReDim bytArray(UBound(valsplited))
            For i = 0 To UBound(valsplited)
                bytArray(i) = CByte(valsplited(i))
                bytArray(i) = CByte(bytArray(i) Xor xORnumber3)
            Next i
            For i = 0 To UBound(valsplited) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber)
            Next i
            For i = 1 To UBound(valsplited) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber2)
            Next i
            OutCode = StrConv(bytArray, vbUnicode)
End Function

Public Function FileCodeIn(FileNum As String, xORnumber As Integer)              '�^�����t�@�C���̈Í���������@���@�t�@�C�����Ɋg���q���Ȃ��A�܂��A�t�@�C�����݂̂̂��́A�{���t���Ă��Ȃ����́i�w���������Ō�������l���K�v�j
Dim Dat() As Byte                '�t�@�C���̃f�[�^
Dim i As Long, Lf As Long      '�J�E���^�ƃt�@�C���T�C�Y
Dim intFilenumber As Integer                '�t�@�C���i���o�[
Dim strexist As String
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer

        strexist = Dir(App.Path & "\" & FileNum)       '�Í�������Ă��Ȃ��t�@�C�������݂���ꍇ
        If strexist = FileNum Then
            xORnumber2 = xORnumber \ 2 + 1        '�Q�x�ڂ�Xor�����ŗ��p����l
            xORnumber3 = xORnumber \ 3 + 2
            '�Í�������t�@�C���̃f�[�^��ǂݍ���
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum For Binary As intFilenumber
                   Lf = LOF(intFilenumber)
                   ReDim Dat(Lf)
                   Get #intFilenumber, 1, Dat
            Close intFilenumber
            
            '�Í�������
            For i = 0 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber)
            Next i
            For i = 1 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber2)
            Next i
            For i = 1 To Lf
                Dat(i) = CByte(Dat(i) Xor xORnumber3)
            Next i
               
            '�Í��������f�[�^���t�@�C���ɏ�������
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum & "�{" For Binary As intFilenumber
            Put #intFilenumber, 1, Dat
            Close intFilenumber
            
            Kill App.Path & "\" & FileNum        '���̃t�@�C�����폜���Ă���
        End If
                    
End Function

Public Function FileCodeOut(FileNum As String, xORnumber As Integer)              '�^�����t�@�C���̕�����������@���t�@�C�����Ɋg���q���Ȃ��A�܂��A�t�@�C�����̂݁i�w���������Ō��������l���K�v�j
Dim Dat() As Byte                '�t�@�C���̃f�[�^
Dim i As Long, Lf As Long      '�J�E���^�ƃt�@�C���T�C�Y
Dim intFilenumber As Integer                '�t�@�C���i���o�[
Dim strexist As String
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer
       
       strexist = Dir(App.Path & "\" & FileNum & "�{")        '�t�@�C�������݂���ꍇ
       If strexist = FileNum & "�{" Then            '�t�@�C�����ɈÍ�������Ă��鎖�������{���t���Ă�����̂����݂���ꍇ
            xORnumber2 = xORnumber \ 2 + 1        '�Q�x�ڂ�Xor�����ŗ��p����l
            xORnumber3 = xORnumber \ 3 + 2
            '����������t�@�C���̃f�[�^��ǂݍ���
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum & "�{" For Binary As intFilenumber
                   Lf = LOF(intFilenumber)
                   ReDim Dat(Lf)
                   Get #intFilenumber, 1, Dat
            Close intFilenumber
            
            '����������
            For i = 1 To Lf
                Dat(i) = CByte(Dat(i) Xor xORnumber3)
            Next i
            For i = 0 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber)
            Next i
            For i = 1 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber2)
            Next i
            
            '�����������f�[�^���t�@�C���ɏ�������
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum For Binary As intFilenumber
            Put #intFilenumber, 1, Dat
            Close intFilenumber
                       
            Kill App.Path & "\" & FileNum & "�{"                  '���̃t�@�C�����폜���Ă���
       End If
End Function




[module1.bas]




Public Type Preserved
    strName As String * 100                 '���[�U�[���ێ��p
    strAddres As String * 100              '�N���C�A���g�̃A�h���X�ێ��p
    intFileAddres As Integer              '�T�[�o���ۑ����Ă���N���C�A���g�f�[�^�t�@�C���̂ǂ��Ɏ����̃f�[�^���ێ�����Ă��邩
    strCulculated As String * 100           '�N���C�A���g���ǂ̒l�܂Ōv�Z�������ێ�
    lngNowCulculate As Long                '�N���C�A���g���O��v�Z�����l
    lngWorkCounts As Long                  '�N���C�A���g�������̃��[�N�������Ȃ�����
    strMassageFrom As String * 1000         '�N���C�A���g���瑗�M���ꂽ���b�Z�[�W
    intNowRange As Integer                '���݂ǂ��͈̔͂܂ŒT���������������i1�Ԗڂ̃��R�[�h�ƈꏏ�ɋL�^�j
    intDistributeCount As Integer         '�Ō�Ɋ���U�����l�́A���񊄂�U������
    strDiscoveredPrime As String * 1000    '���݂܂łɔ��������f��
    intMemorySize As Integer              '�N���C�A���g�̕�������������
    intCpuClock As Single                 '�N���C�A���g�̂b�o�t�̃N���b�N��
    strCpuType As String * 100              '�N���C�A���g�̂b�o�t�̎��
    intPrize As Integer                    '�ڑ����󂯂����_�ł̏���
    lngNowCulculateArray As Long            '�N���C�A���g���v�Z���Ă���w����ێ����Ă���PreparePrime�̂P�����ڂ̓Y����
    lngClientWorkingTime As Long            '�N���C�A���g�̑��N�����Ԃ�b�P�ʂŕێ�
End Type

Public Const xORnumber = 173         '�f�[�^���Í������鎞��xor�����Ō�������l

Public Type Confirmed
    lngPrimeNumber As Long                          '�w�����ێ�����Ă���prepareprime�̈ꎟ���ڂ̓Y����
    intTrueOrFalse As Integer                       '�f���ł��邩�Ȃ����@1=true 2=false
End Type

Declare Function lstrlen Lib "kernel32" Alias "lstrlenA" (ByVal lpString As String) As Long    ' lstrlen()�֐��@������̃o�C�g���𓾂�`�o�h�̐錾
Declare Sub MoveMemory Lib "kernel32" Alias "RtlMoveMemory" (pDest As Any, pSource As Any, ByVal ByteLen As Long)      ' ����������֐��@��������o�C�g�z��ɑ������
