# Copyrights 2010-2014 by [Mark Overmeer].
#  For other contributors see ChangeLog.
# See the manual pages for details on the licensing terms.
# Pod stripped from pm file by OODoc 2.01.
use warnings;
use strict;

package XML::eXistDB::Util;
our $VERSION = '0.13';

use base 'Exporter';


our @EXPORT = qw/
    NS_COLLECTION_XCONF 
    NS_EXISTDB
    /;

use constant
  { NS_COLLECTION_XCONF => 'http://exist-db.org/collection-config/1.0'
  , NS_EXISTDB          => 'http://exist.sourceforge.net/NS/exist'
  };

1;
