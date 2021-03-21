#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

int isprime(char *n);
char *doublePow(long int r);
char *doubleplusminus(char *strone, char *strtwo);
char *doublemultiply(char *strone, char *strtwo);
char *doubledivision(char *strone, char *strtwo);
char *doublemod(char *strone, char *strtwo);
void mid(char *receive, char *back, int a, int b);
int doubleLeftB(char *strone, char *strtwo);
int doubleRightB(char *strone, char *strtwo);
int doubleEqual(char *strone, char *strtwo);
char *doubleINT(char *str);

char *doublePow(char *n, long int r)
{ /*nのｒ乗を返す**/
	char t[100000] = {"1"};
	char x[100000];

	strcpy(x, n);
	for (; r > 0; r >>= 1, strcpy(x, doublemultiply(x, x)))
	{
		if (r & 1)
			strcpy(t, doubleINT(doublemultiply(t, x)));
	}
	mid(t, t, 1, strcspn(t, "."));
	return t;
}

char *doubleplusminus(char *strone, char *strtwo)
{ //多倍長加算関数（引数は文字列が２つ）
	char *a, *b, *c;
	int i, j, na, nb, nmax, nmin, up, work, x, y, periodonea, periodtwoa, periodoneb, periodtwob, periodposition;
	char *str1, *str2, *str3, *tstr, *strtempolaly, *strtempolaly2;
	char minusflag1, minusflag2, tempolalyflag;

	na = strlen(strone);
	nb = strlen(strtwo);
	if (na >= nb)
	{
		nmax = na;
	}
	else
	{
		nmax = nb;
	}
	work = (nmax * 4) * sizeof(char);
	strtempolaly = (char *)malloc(work);
	strtempolaly2 = (char *)malloc(work);
	str1 = (char *)malloc(work);
	str2 = (char *)malloc(work);
	strcpy(str1, strone);
	strcpy(str2, strtwo);
	if (str1[0] == '-' && str1[1] == '-')
	{
		for (x = 2; x <= strlen(str1); x++)
		{
			str1[x - 2] = str1[x];
		}
	}
	if (str2[0] == '-' && str2[1] == '-')
	{
		for (x = 2; x <= strlen(str2); x++)
		{
			str2[x - 2] = str2[x];
		}
	}
	minusflag1 = 0;
	minusflag2 = 0;
	strcpy(strtempolaly, str1);
	strcpy(strtempolaly2, str2);
	if (str1[0] == '-')
	{
		minusflag1 = 1;
		for (x = 1; x <= strlen(str1); x++)
		{
			str1[x - 1] = str1[x];
		}
	}
	if (str2[0] == '-')
	{
		minusflag2 = 1;
		for (x = 1; x <= strlen(str2); x++)
		{
			str2[x - 1] = str2[x];
		}
	}
	if (strcmp(str1, "0") == 0)
	{
		return strtempolaly2;
	}
	if (strcmp(str2, "0") == 0)
	{
		return strtempolaly;
	}

	a = (char *)malloc(work);
	b = (char *)malloc(work);
	c = (char *)malloc(work);
	str3 = (char *)malloc(work);
	tstr = (char *)malloc(work);

	periodoneb = strcspn(str1, ".") + 1;
	periodtwob = strcspn(str2, ".") + 1;
	if (periodoneb != strlen(str1) + 1)
	{ /*小数点を取り除く*/
		periodonea = strlen(str1) - periodoneb;
		mid(str1, strtempolaly, 1, periodoneb - 1);
		mid(str1, strtempolaly2, periodoneb + 1, strlen(str1) - periodoneb);
		strcat(strtempolaly, strtempolaly2);
		strcpy(str1, strtempolaly);
	}
	else
	{
		periodoneb = 0;
		periodonea = 0;
	}
	if (periodtwob != strlen(str2) + 1)
	{
		periodtwoa = strlen(str2) - periodtwob;
		mid(str2, strtempolaly, 1, periodtwob - 1);
		mid(str2, strtempolaly2, periodtwob + 1, strlen(str2) - periodtwob);
		strcat(strtempolaly, strtempolaly2);
		strcpy(str2, strtempolaly);
	}
	else
	{
		periodtwob = 0;
		periodtwoa = 0;
	}
	if ((periodoneb != 0) || (periodtwob != 0))
	{
		if (periodonea > periodtwoa)
		{
			for (x = 1; x <= periodonea - periodtwoa; x++)
			{
				strcat(str2, "0");
			}
			periodposition = periodonea;
		}
		else if (periodtwoa > periodonea)
		{
			for (x = 1; x <= periodtwoa - periodonea; x++)
			{
				strcat(str1, "0");
			}
			periodposition = periodtwoa;
		}
		else
		{
			periodposition = periodonea;
		}
	}
	else
	{
		periodposition = 0;
	}

	if (strlen(str2) > strlen(str1))
	{
		strcpy(strtempolaly, str1);
		tempolalyflag = minusflag1;
		strcpy(str1, str2);
		minusflag1 = minusflag2;
		strcpy(str2, strtempolaly);
		minusflag2 = tempolalyflag;
	}
	else if (strlen(str1) == strlen(str2))
	{
		for (x = 0; x <= strlen(str1) - 1; x++)
		{
			if (str1[x] - '0' < str2[x] - '0')
			{
				strcpy(strtempolaly, str1);
				tempolalyflag = minusflag1;
				strcpy(str1, str2);
				minusflag1 = minusflag2;
				strcpy(str2, strtempolaly);
				minusflag2 = tempolalyflag;
				break;
			}
			else if (str1[x] - '0' > str2[x] - '0')
			{
				break;
			}
		}
	}

	j = strlen(str1) + 1;
	na = j;
	for (i = j; i >= 2; i--)
	{
		a[j - i + 1] = str1[i - 2] - '0';
	}
	j = strlen(str2) + 1;
	nb = j;
	if (minusflag1 == minusflag2)
	{
		for (i = j; i >= 2; i--)
		{
			b[j - i + 1] = str2[i - 2] - '0';
		}
	}
	else
	{
		for (i = j; i >= 2; i--)
		{
			b[j - i + 1] = -1 * (str2[i - 2] - '0');
		}
	}
	if (na > nb)
	{
		nmax = na - 1;
		nmin = nb - 1;
	}
	else
	{
		nmax = nb - 1;
		nmin = na - 1;
	}
	for (i = nmin + 1; i <= nmax; i++)
	{
		if (nmax != nmin)
		{
			b[i] = 0;
		}
	}

	up = 0;
	for (i = 1; i <= nmax; i++)
	{
		work = a[i] + b[i] + up;
		if (work >= 10)
		{
			up = 1;
			work = work - 10;
		}
		else if (work < 0)
		{
			up = -1;
			work = work + 10;
		}
		else
		{
			up = 0;
		}
		c[i] = work;
	}
	c[nmax + 1] = up;

	strcpy(tstr, "");
	strcpy(str3, "");
	j = 0;
	for (i = nmax + 1; i >= 1; i--)
	{
		sprintf(str3, "%s%d", str3, c[i]);
	}

	if (periodposition != 0)
	{
		mid(str3, str1, 1, strlen(str3) - periodposition);
		mid(str3, str2, strlen(str3) - periodposition + 1, periodposition);
		strcat(str1, ".");
		strcat(str1, str2);
		strcpy(str3, str1);
		for (x = 1; x <= periodposition; x++)
		{
			mid(str3, str1, strlen(str3), 1);
			if (strcmp(str1, "0") == 0)
			{
				mid(str3, str3, 1, strlen(str3) - 1);
			}
			else
			{
				break;
			}
		}
		mid(str3, str1, strlen(str3), 1);
		if (strcmp(str1, ".") == 0)
		{
			mid(str3, str3, 1, strlen(str3) - 1);
		}
	}
	periodposition = strcspn(str3, ".") + 1;
	if (periodposition != strlen(str3) + 1)
	{
		for (x = 1; x <= periodposition - 2; x++)
		{
			mid(str3, str1, 1, 1);
			if (strcmp(str1, "0") == 0)
			{
				mid(str3, strtempolaly, 2, strlen(str3) - 1);
				strcpy(str3, strtempolaly);
			}
			else
			{
				break;
			}
		}
	}
	else
	{
		while (1)
		{
			mid(str3, str1, 1, 1);
			if (strcmp(str1, "0") == 0)
			{
				if (strlen(str3) == 1)
				{
					break;
				}
				work = strlen(str3) - 1;
				mid(str3, strtempolaly, 2, work);
				strcpy(str3, strtempolaly);
			}
			else
			{
				break;
			}
		}
	}

	if (strcmp(str3, "") == 0)
	{
		strcpy(str3, "0");
		return str3;
	}
	free(a);
	free(b);
	free(c);
	free(str1);
	free(str2);
	free(tstr);
	free(strtempolaly2);
	if (minusflag1 == 1 && minusflag2 == 1)
	{
		strcpy(strtempolaly, "-");
		strcpy(str3, strcat(strtempolaly, str3));
		free(strtempolaly);
		return str3;
	}
	else if ((minusflag1 != minusflag2) && minusflag1 == 1)
	{
		strcpy(strtempolaly, "-");
		strcpy(str3, strcat(strtempolaly, str3));
		free(strtempolaly);
		return str3;
	}
	else
	{
		free(strtempolaly);
		return str3;
	}
}

char *doublemultiply(char *strone, char *strtwo)
{ //多倍長乗算関数（引数は文字列が２つ）
	char *a, *b, *c, *d;
	int i, j, na, nb, up, work, x, y, arraylimit, tmod, periodone, periodtwo, periodposition;
	char *str1, *str2, *str3, *tstr, *strtempolaly, *strtempolaly2;
	char minusflag1, minusflag2;

	if (strcmp(strone, "0") == 0)
	{
		return "0";
	}
	if (strcmp(strtwo, "0") == 0)
	{
		return "0";
	}
	if (strcmp(strone, "1") == 0)
	{
		return strtwo;
	}
	if (strcmp(strtwo, "1") == 0)
	{
		return strone;
	}
	na = strlen(strone);
	nb = strlen(strtwo);
	work = ((na + nb) * 4) * sizeof(char);
	strtempolaly = (char *)malloc(work);
	strtempolaly2 = (char *)malloc(work);
	str1 = (char *)malloc(work);
	str2 = (char *)malloc(work);
	strcpy(str1, strone);
	strcpy(str2, strtwo);
	if (str1[0] == '-' && str1[1] == '-')
	{
		for (x = 2; x <= strlen(str1); x++)
		{
			str1[x - 2] = str1[x];
		}
	}
	if (str2[0] == '-' && str2[1] == '-')
	{
		for (x = 2; x <= strlen(str2); x++)
		{
			str2[x - 2] = str2[x];
		}
	}
	if (na + nb <= 6)
	{
		sprintf(strtempolaly, "%lf", atof(str1) * atof(str2));
		return strtempolaly;
	}
	strtempolaly2 = (char *)malloc(work);
	a = (char *)malloc((na + 3) * 2 * sizeof(char));
	b = (char *)malloc((nb + 3) * 2 * sizeof(char));
	c = (char *)malloc(work);
	d = (char *)malloc(work);
	str3 = (char *)malloc(work);
	tstr = (char *)malloc(work);
	minusflag1 = 0;
	minusflag2 = 0;
	if (str1[0] == '-')
	{
		minusflag1 = 1;
		for (x = 1; x <= strlen(str1); x++)
		{
			str1[x - 1] = str1[x];
		}
	}
	if (str2[0] == '-')
	{
		minusflag2 = 1;
		for (x = 1; x <= strlen(str2); x++)
		{
			str2[x - 1] = str2[x];
		}
	}
	periodone = strcspn(str1, ".") + 1;
	periodtwo = strcspn(str2, ".") + 1;

	if ((periodone != strlen(str1) + 1) && (periodtwo != strlen(str2) + 1))
	{
		periodposition = strlen(str1) + strlen(str2) - periodone - periodtwo;
	}
	else if (periodone != strlen(str1) + 1)
	{
		periodposition = strlen(str1) - periodone;
	}
	else if (periodtwo != strlen(str2) + 1)
	{
		periodposition = strlen(str2) - periodtwo;
	}
	else
	{
		periodposition = 0;
	}
	if (periodone != strlen(str1) + 1)
	{ /*小数点を取り除く*/
		mid(str1, strtempolaly, 1, periodone - 1);
		mid(str1, strtempolaly2, periodone + 1, strlen(str1) - periodone);
		strcat(strtempolaly, strtempolaly2);
		strcpy(str1, strtempolaly);
	}
	if (periodtwo != strlen(str2) + 1)
	{
		mid(str2, strtempolaly, 1, periodtwo - 1);
		mid(str2, strtempolaly2, periodtwo + 1, strlen(str2) - periodtwo);
		strcat(strtempolaly, strtempolaly2);
		strcpy(str2, strtempolaly);
	}

	j = strlen(str1) + 1;
	na = j;
	for (i = j; i >= 2; i--)
	{
		a[j - i + 1] = str1[i - 2] - '0';
	}
	j = strlen(str2) + 1;
	nb = j;
	for (i = j; i >= 2; i--)
	{
		b[j - i + 1] = str2[i - 2] - '0';
	}

	work = na + nb + 2;
	for (i = 1; i <= work; i++)
	{
		c[i] = 0;
		d[i] = 0;
	}

	na = strlen(str1);
	nb = strlen(str2);
	for (i = 1; i <= nb; i++)
	{
		up = 0;
		for (j = 1; j <= na; j++)
		{
			work = a[j] * b[i] + up;
			up = 0;
			if (work >= 10)
			{
				up = work / 10;
				d[i + j - 1] = work % 10;
			}
			else
			{
				d[i + j - 1] = work;
			}
		}
		if (up != 0)
		{
			d[i + j - 1] = up;
			arraylimit = i + j - 1;
		}
		else
		{
			arraylimit = i + j - 2;
		}
		for (x = 1; x <= arraylimit; x++)
		{
			c[x] = c[x] + d[x];
			y = x;
			while (c[y] >= 10)
			{
				tmod = c[y] / 10;
				c[y + 1] = c[y + 1] + tmod;
				c[y] = c[y] % 10;
				y++;
			}
			d[x] = 0;
		}
	}

	strcpy(tstr, "");
	strcpy(str3, "");
	for (i = arraylimit; i >= 1; i--)
	{
		sprintf(str3, "%s%d", str3, c[i]);
	}

	if (periodposition != 0)
	{
		mid(str3, str1, 1, strlen(str3) - periodposition);
		mid(str3, str2, strlen(str3) - periodposition + 1, periodposition);
		strcat(str1, ".");
		strcat(str1, str2);
		strcpy(str3, str1);
		for (x = 1; x <= periodposition; x++)
		{
			mid(str3, str1, strlen(str3), 1);
			if (strcmp(str1, "0") == 0)
			{
				mid(str3, str3, 1, strlen(str3) - 1);
			}
			else
			{
				break;
			}
		}
		mid(str3, str1, strlen(str3), 1);
		if (strcmp(str1, ".") == 0)
		{
			mid(str3, str3, 1, strlen(str3) - 1);
		}
	}
	periodposition = strcspn(str3, ".") + 1;
	if (periodposition != strlen(str3) + 1)
	{
		for (x = 1; x <= periodposition - 2; x++)
		{
			mid(str3, str1, 1, 1);
			if (strcmp(str1, "0") == 0)
			{
				mid(str3, strtempolaly, 2, strlen(str3) - 1);
				strcpy(str3, strtempolaly);
			}
			else
			{
				break;
			}
		}
	}
	else
	{
		while (1)
		{
			mid(str3, str1, 1, 1);
			if (strcmp(str1, "0") == 0)
			{
				if (strlen(str3) == 1)
				{
					break;
				}
				mid(str3, strtempolaly, 2, strlen(str3) - 1);
				strcpy(str3, strtempolaly);
			}
			else
			{
				break;
			}
		}
	}
	free(a);
	free(b);
	free(c);
	free(d);
	free(str1);
	free(str2);
	free(tstr);
	free(strtempolaly2);

	if (strcmp(str3, "") == 0)
	{
		free(strtempolaly);
		strcpy(str3, "0");
		return str3;
	}
	if (minusflag1 != minusflag2)
	{
		strcpy(strtempolaly, "-");
		strcpy(str3, strcat(strtempolaly, str3));
		free(strtempolaly);
		return str3;
	}
	else
	{
		free(strtempolaly);
		return str3;
	}
}

char *doubledivision(char *strone, char *strtwo)
{ //多倍長除算関数（引数は文字列が２つ）
	int i, j, na, nb, up, work, x, y, arraylimit, tmod, periodone, periodtwo, periodposition, effective, lasteffective;
	char *strtemporaly, *strtemporaly2, *strnewtonstart, *strtemporalydivi, *str1, *str2;
	char minusflag1, minusflag2, strcompare[3];

	if (strcmp(strtwo, "1") == 0)
	{
		return strone;
	}
	str1 = (char *)malloc((strlen(strone) + 5) * 2 * sizeof(char));
	str2 = (char *)malloc((strlen(strtwo) + 5) * 2 * sizeof(char));
	strcpy(str1, strone);
	strcpy(str2, strtwo);
	if (str1[0] == '-' && str1[1] == '-')
	{
		for (x = 2; x <= strlen(str1); x++)
		{
			str1[x - 2] = str1[x];
		}
	}
	if (str2[0] == '-' && str2[1] == '-')
	{
		for (x = 2; x <= strlen(str2); x++)
		{
			str2[x - 2] = str2[x];
		}
	}
	na = strlen(str1);
	nb = strlen(str2);
	strtemporaly = (char *)malloc(25 * sizeof(char));
	minusflag1 = 0;
	minusflag2 = 0;
	if (str1[0] == '-')
	{
		minusflag1 = 1;
		for (x = 1; x <= strlen(str1); x++)
		{
			str1[x - 1] = str1[x];
		}
	}
	if (str2[0] == '-')
	{
		minusflag2 = 1;
		for (x = 1; x <= strlen(str2); x++)
		{
			str2[x - 1] = str2[x];
		}
	}
	work = (strlen(str2) + 10) * 2 * sizeof(char);
	strtemporaly = (char *)realloc(strtemporaly, work);
	strtemporalydivi = (char *)malloc(work);
	strnewtonstart = (char *)malloc(work);
	periodtwo = strcspn(str2, ".") + 1;
	for (x = 1; x <= strlen(str2); x++)
	{
		mid(str2, strcompare, x, 1);
		if ((strcmp(strcompare, ".") != 0) && (strcmp(strcompare, "0") != 0))
		{
			if (strcmp(strcompare, "1") == 0)
			{
				if (x < periodtwo && periodtwo != strlen(str2) + 1)
				{
					strcpy(strnewtonstart, "0.");
					for (y = 1; y <= periodtwo - x - 1; x++)
					{
						strcat(strnewtonstart, "0");
					}
					strcat(strnewtonstart, "9");
				}
				else if (x > periodtwo && periodtwo != strlen(str2) + 1)
				{
					strcpy(strnewtonstart, "9");
					for (y = 1; y <= x - periodtwo - 1; x++)
					{
						strcat(strnewtonstart, "0");
					}
				}
				else if (periodtwo == strlen(str2) + 1)
				{
					strcpy(strnewtonstart, "0.");
					for (y = 1; y <= periodtwo - x - 1; x++)
					{
						strcat(strnewtonstart, "0");
					}
					strcat(strnewtonstart, "9");
				}
				break;
			}
			else
			{
				sprintf(strtemporaly, "%d", 10 / (strcompare[0] - '0'));
				if (x < periodtwo && periodtwo != strlen(str2) + 1)
				{
					strcpy(strnewtonstart, "0.");
					for (y = 1; y <= periodtwo - x - 1; x++)
					{
						strcat(strnewtonstart, "0");
					}
					strcat(strnewtonstart, strtemporaly);
				}
				else if (x > periodtwo && periodtwo != strlen(str2) + 1)
				{
					strcpy(strnewtonstart, strtemporaly);
					for (y = 1; y <= x - periodtwo - 1; x++)
					{
						strcat(strnewtonstart, "0");
					}
				}
				else if (periodtwo == strlen(str2) + 1)
				{
					strcpy(strnewtonstart, "0.");
					for (y = 1; y <= periodtwo - x - 1; x++)
					{
						strcat(strnewtonstart, "0");
					}
					strcat(strnewtonstart, strtemporaly);
				}
				break;
			}
		}
	}

	strcpy(strtemporalydivi, strnewtonstart);
	work = strlen(str1);
	if (strlen(str2) > work)
	{
		work = strlen(str2);
	}
	effective = work * 4;
	lasteffective = work * 3;
	work = work * 32;
	strtemporaly = (char *)realloc(strtemporaly, work * sizeof(char));
	strtemporaly2 = (char *)malloc(work * sizeof(char));
	strtemporalydivi = (char *)realloc(strtemporalydivi, work * sizeof(char));
	for (x = 1; x <= 10; x++)
	{
		strcpy(strtemporaly, "-");
		strcat(strtemporaly, doublemultiply(str2, doublemultiply(strtemporalydivi, strtemporalydivi)));
		strcpy(strtemporaly2, doublemultiply("2", strtemporalydivi));
		strcpy(strtemporalydivi, doubleplusminus(strtemporaly2, strtemporaly));
		if (strlen(strtemporalydivi) > effective)
		{
			mid(strtemporalydivi, strtemporalydivi, 1, effective);
		}
	}

	strcpy(strtemporalydivi, doublemultiply(str1, strtemporalydivi));
	if (strlen(strtemporalydivi) > lasteffective)
	{
		mid(strtemporalydivi, strtemporaly, 1, lasteffective);
		strcpy(strtemporalydivi, strtemporaly);
	}
	mid(strtemporalydivi, strtemporaly, 1, 1);
	if (strcmp(strtemporaly, "-") == 0)
	{
		mid(strtemporalydivi, strtemporaly, 2, strlen(strtemporalydivi) - 1);
		strcpy(strtemporalydivi, strtemporaly);
	}
	free(str2);
	free(strnewtonstart);
	free(strtemporaly2);
	free(str1);
	if (strcmp(strtemporalydivi, "") == 0)
	{
		free(strtemporaly);
		strcpy(strtemporalydivi, "0");
		return strtemporalydivi;
	}
	if (minusflag1 != minusflag2)
	{
		strcpy(strtemporaly, "-");
		strcpy(strtemporalydivi, strcat(strtemporaly, strtemporalydivi));
		free(strtemporaly);
		return strtemporalydivi;
	}
	else
	{
		free(strtemporaly);
		return strtemporalydivi;
	}
}

char *doubleINT(char *str)
{ /*小数点以下を切り捨てる*/

	char *strtemporaly;
	strtemporaly = (char *)malloc(strlen(str) + 5);
	mid(str, strtemporaly, 1, strcspn(str, "."));
	return strtemporaly;
}

char *doublemod(char *strone, char *strtwo)
{ //多倍長剰余演算関数（引数は正の整数の文字列が２つ）
	int na, nb, work;
	char *strtempolaly, *strtempolaly2;

	if (strcmp(strone, strtwo) == 0)
	{
		return "0";
	}
	if (strcmp(strtwo, "1") == 0)
	{
		return "0";
	}
	na = strlen(strone);
	nb = strlen(strtwo);
	work = ((na + nb) * 10);
	strtempolaly = (char *)malloc(work);
	strtempolaly2 = (char *)malloc(work);
	strtempolaly = doubledivision(strone, strtwo);
	mid(strtempolaly, strtempolaly2, 1, strcspn(strtempolaly, "."));
	strtempolaly = doublemultiply(strtwo, strtempolaly2);
	strcpy(strtempolaly2, "-");
	strcat(strtempolaly2, strtempolaly);
	strtempolaly = doubleplusminus(strone, strtempolaly2);
	free(strtempolaly2);
	if (strcmp(strtempolaly, strtwo) == 0)
	{
		return "0";
	}
	return strtempolaly;
}

int doubleLeftB(char *strone, char *strtwo)
{ /*比較演算子（>）の関数、偽の時を真とするならば(=<)として利用可能*/
	int na, nb, work, periodoneb, periodtwob, periodonea, periodtwoa, nmax, x;
	char *strtemporaly, *strtemporaly2, *str1, *str2;
	na = strlen(strone);
	nb = strlen(strtwo);
	if (na >= nb)
	{
		nmax = na;
	}
	else
	{
		nmax = nb;
	}
	work = (nmax * 4) * sizeof(char);
	strtemporaly = (char *)malloc(work);
	strtemporaly2 = (char *)malloc(work);
	str1 = (char *)malloc(work);
	str2 = (char *)malloc(work);
	strcpy(str1, strone);
	strcpy(str2, strtwo);

	periodoneb = strcspn(str1, ".") + 1;
	periodtwob = strcspn(str2, ".") + 1;
	if (periodoneb != strlen(str1) + 1)
	{ /*小数点を取り除く*/
		periodonea = strlen(str1) - periodoneb;
		mid(str1, strtemporaly, 1, periodoneb - 1);
		mid(str1, strtemporaly2, periodoneb + 1, strlen(str1) - periodoneb);
		strcat(strtemporaly, strtemporaly2);
		strcpy(str1, strtemporaly);
	}
	else
	{
		periodoneb = 0;
		periodonea = 0;
	}
	if (periodtwob != strlen(str2) + 1)
	{
		periodtwoa = strlen(str2) - periodtwob;
		mid(str2, strtemporaly, 1, periodtwob - 1);
		mid(str2, strtemporaly2, periodtwob + 1, strlen(str2) - periodtwob);
		strcat(strtemporaly, strtemporaly2);
		strcpy(str2, strtemporaly);
	}
	else
	{
		periodtwob = 0;
		periodtwoa = 0;
	}
	if ((periodoneb != 0) || (periodtwob != 0))
	{
		if (periodonea > periodtwoa)
		{
			for (x = 1; x <= periodonea - periodtwoa; x++)
			{
				strcat(str2, "0");
			}
		}
		else if (periodtwoa > periodonea)
		{
			for (x = 1; x <= periodtwoa - periodonea; x++)
			{
				strcat(str1, "0");
			}
		}
	}

	free(strtemporaly);
	free(strtemporaly2);
	if (strlen(str2) > strlen(str1))
	{
		free(str1);
		free(str2);
		return 0;
	}
	else if (strlen(str1) > strlen(str2))
	{
		free(str1);
		free(str2);
		return 1;
	}
	else if (strlen(str1) == strlen(str2))
	{
		for (x = 0; x <= strlen(str1) - 1; x++)
		{
			if (str1[x] - '0' < str2[x] - '0')
			{
				free(str1);
				free(str2);
				return 0;
			}
			else if (str1[x] - '0' > str2[x] - '0')
			{
				free(str1);
				free(str2);
				return 1;
			}
		}
	}
	free(str1);
	free(str2);
	return 0;
}

int doubleRightB(char *strone, char *strtwo)
{ /*比較演算子（<）の関数、偽の時を真とするならば(=>)として利用可能*/
	int na, nb, work, periodoneb, periodtwob, periodonea, periodtwoa, nmax, x;
	char *strtemporaly, *strtemporaly2, *str1, *str2;
	na = strlen(strone);
	nb = strlen(strtwo);
	if (na >= nb)
	{
		nmax = na;
	}
	else
	{
		nmax = nb;
	}
	work = (nmax * 4) * sizeof(char);
	strtemporaly = (char *)malloc(work);
	strtemporaly2 = (char *)malloc(work);
	str1 = (char *)malloc(work);
	str2 = (char *)malloc(work);
	strcpy(str1, strone);
	strcpy(str2, strtwo);

	periodoneb = strcspn(str1, ".") + 1;
	periodtwob = strcspn(str2, ".") + 1;
	if (periodoneb != strlen(str1) + 1)
	{ /*小数点を取り除く*/
		periodonea = strlen(str1) - periodoneb;
		mid(str1, strtemporaly, 1, periodoneb - 1);
		mid(str1, strtemporaly2, periodoneb + 1, strlen(str1) - periodoneb);
		strcat(strtemporaly, strtemporaly2);
		strcpy(str1, strtemporaly);
	}
	else
	{
		periodoneb = 0;
		periodonea = 0;
	}
	if (periodtwob != strlen(str2) + 1)
	{
		periodtwoa = strlen(str2) - periodtwob;
		mid(str2, strtemporaly, 1, periodtwob - 1);
		mid(str2, strtemporaly2, periodtwob + 1, strlen(str2) - periodtwob);
		strcat(strtemporaly, strtemporaly2);
		strcpy(str2, strtemporaly);
	}
	else
	{
		periodtwob = 0;
		periodtwoa = 0;
	}
	if ((periodoneb != 0) || (periodtwob != 0))
	{
		if (periodonea > periodtwoa)
		{
			for (x = 1; x <= periodonea - periodtwoa; x++)
			{
				strcat(str2, "0");
			}
		}
		else if (periodtwoa > periodonea)
		{
			for (x = 1; x <= periodtwoa - periodonea; x++)
			{
				strcat(str1, "0");
			}
		}
	}

	free(strtemporaly);
	free(strtemporaly2);
	if (strlen(str2) > strlen(str1))
	{
		free(str1);
		free(str2);
		return 1;
	}
	else if (strlen(str1) > strlen(str2))
	{
		free(str1);
		free(str2);
		return 0;
	}
	else if (strlen(str1) == strlen(str2))
	{
		for (x = 0; x <= strlen(str1) - 1; x++)
		{
			if (str1[x] - '0' < str2[x] - '0')
			{
				free(str1);
				free(str2);
				return 1;
			}
			else if (str1[x] - '0' > str2[x] - '0')
			{
				free(str1);
				free(str2);
				return 0;
			}
		}
	}
	free(str1);
	free(str2);
	return 0;
}

int doubleEqual(char *strone, char *strtwo)
{ /*等価演算子（＝＝）の関数*/

	if (strcmp(strone, strtwo) == 0)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

void mid(char *receive, char *back, int a, int b)
{
	int k, n;

	n = strlen(receive);
	if ((0 < a && a <= n) && (0 < b && b <= n) && (a + b - 1 <= n))
	{
		for (k = a; k < a + b; k++)
		{
			back[k - a] = receive[k - 1];
			back[b] = '\0';
		}
	}
	else
	{
		back[0] = '\0';
	}
}
