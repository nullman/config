#!/usr/bin/perl
# erzeugt aus kaddressbook ldif export file import file f. LDAP

# adapt to your base settings
$mydomain = "mydomain";
$myext = "com";

#exportfile (LDIF-Format) - 1. Commandlineargument
 $file=$ARGV[0];

open (LDIF, "< $file") || die "can't open $file.";
$lastrecord = "";
while (<LDIF>)
{
	$record=$_;
	chomp($record);
	$skip = 0;
	# skip unwanted stuff
	if(index($record,"uid:") >= 0)	{ $skip = 1; }
	if(index($record,"modifytimestamp:") >= 0)	{ $skip = 1; }
	if(index($record,"secondemail:") >= 0)	{ $skip = 1; }
	if(index($record,"homeurl:") >= 0)	{ $skip = 1; }
	if(index($record,"o:") >= 0)	{ $skip = 1; }
	if(index($record,"organization:") >= 0)	{ $skip = 1; }
	if(index($record,"organizationname:") >= 0)	{ $skip = 1; }
	if(index($record,"nickname:") >= 0)	{ $skip = 1; }
	if(index($record,"description:") >= 0)	{ $skip = 1; }
	if(index($record,"countryname:") >= 0)	{ $skip = 1; }
	if(index($record,"vocation:") >= 0)	{ $skip = 1; }
	# things to replace/blank out
	$record =~ s/mozilla//gi;
	$record =~ s/workurl:/labeledURI:/gi;
	$record =~ s/cellphone:/mobile:/gi;
	$record =~ s/streethomeaddress:/postalAddress:/gi;
	$record =~ s/streetaddress:/street:/gi;
	$record =~ s/homepostalcode:/postalcode:/gi;
	$record =~ s/homepostaladdress:/postaladdress:/gi;
	$record =~ s/homelocalityname:/l:/gi;
	if(index($record,"locality:") >= 0)	{ $skip = 1; }
	# skip=1 means this record will be ignored
	if ($skip == 0)	{
		$mi = index($record,",mail=");
		if($mi >= 0)	{	# cut off 'mail=..' in 'dn: ..' line
			$record = substr($record,0,$mi);
		}
		if(index($record,"dn: cn=") >= 0)	{	# process commas ',' in 'dn: cn..' line
			@Felder=split(/=/,$record,40);
			@CN = split(/, /,$Felder[1],40);
			$Name = $CN[0];
			$VorName = $CN[1];
			$Vorname =~ s/ //g;
			$record = "dn: cn="."$Name $VorName,dc=$mydomain,dc=$myext";
		}
		if(index($record,"cn: ") >= 0)	{	# process commas ',' in 'cn: ..' line
			@Felder=split(/: /,$record,40);
			@CN = split(/, /,$Felder[1],40);
			$Name = $CN[0];
			$VorName = $CN[1];
			$Vorname =~ s/ //g;
			$record = "cn: "."$Name $VorName";
		}
		$L = index($lastrecord,$record);
		if(($L < 0))	{	#avoid 'double' entries (only if successive!)
			print "$record\n";
		}
		if (length($record) == 0)	{	# append this objectclass to every record (needed for 'mail' attribute to be imported)
			print "objectclass: inetOrgPerson\n\n";
		}
		$lastrecord = $record;
	}
}
close(LDIF);
