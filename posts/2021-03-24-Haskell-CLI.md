---
title: Designing Command Line Interfaces in Haskell
isBlogPost: True
image: images/controller.jpg
---

![](../images/controller.jpg)

Command line tools are essential in many computational fields of science, not the least because command lines are often the only interface on a high-performance compute cluster or server. Particularly critical in command-line tools is a well-defined interface between the user and the program, the command line interface (CLI). A command line interface is like a contract. The user promises to provide input parameters in the right shape and type, and the tool promises to work with this input the way it is expected to. A breach of contract from the caller may result - at best - in a refusal of the program to run, and at worst in undefined behavior. 

A well-defined command line interface helps ensuring loyalty to contract for a user. Moreover, a well-defined interface exposes to the user just the right amount of power to control the program. This is more tricky than it sounds: It is not enough to expose dozens of options and flags for the user to set, there also has to be some evaluation logic to ensure the combination of options and flags makes sense. As I hope to be able to convince you in this post, a strict and expressive typing system like in Haskell is a huge help with that.

There's going to be quite some Haskell code in this post, and I've written it in part to get people interested in the language and the advantages of strong types. If you find this post interesting enough to give Haskell a chance, I recommend the freely available book and tutorial [Learn You a Haskell for Great Good](http://learnyouahaskell.com). The sourcecode described here can also be viewed in full in a [dedicated github-repository](https://github.com/stschiff/haskell-cli-example).

## Specifying the interface through types

OK, let's dive in. Let's consider a program that reads in a file and computes and outputs summary statistics from that file. Even though we're not coding anything beyond the CLI, consider for the sake of concreteness a bioinformatic program that reads genotypes for a number of individuals and outputs summary statistics for each individual.

In Haskell, the first step to designing a CLI is to define a data structure that captures all possible options for our program:

```haskell
data Options = Options {
	missingnessThreshold :: Double,
	verbose              :: Bool,
	summaryStat          :: SummaryStatSpec,
	inputFormat          :: FormatSpec,
	individuals          :: IndividualsSpec
}

data SummaryStatSpec = Heterozygosity
					 | SegregatingSites
					 | HardyWeinbergDeviation

data FormatSpec = PlinkFormat FilePath FilePath FilePath
			    | VCFFormat FilePath

data IndividualsSpec = IndividualsByFile FilePath
					 | IndividualsByList [String]

```

OK, so what's going on here? First, we defined a record type called `Options`, which has five fields, called `missingnessThreshold`, `verbose`, and so on. Each record has a type. Some types should sound familiar, such as `Double` (a decimal number) or `Bool` (True or False), which here define a missingness cutoff filter (exclude individuals with too much missing genotypes) and a verbosity flag (if `True`, print out extra log messages).

The three other types are themselves custom types, defined below the record type. `SummaryStatSpec` represents different genome-wide summary statistic types that the user might want to compute for the specified individuals. The type is a simple enumeration of different values(`Heterozygosity`, `SegregatingSites` and `HardyWeinbergDeviation`), separated in Haskell using the pipe (`|`) operator. A value of type `SummaryStatSpec` has to be strictly one of these three choices. Notice how the type itself already guarantees exclusivity: the `summaryStatSpec` field cannot be simultaneously `Heterozyosity` and `SegregatingSites`.

Similarly, `FormatSpec` defines two alternatives `PlinkFormat` and `VCFFormat` (separated as above by the pipe operator `|`). However, now we have some additional values defined here. A `PlinkFormat` value is associated with three filenames of type `FilePath`. The alternative, `VCFFormat`, is associated with one filename. This may become clearer with an example:

```haskell
f :: FormatSpec
f = PlinkFormat "file.geno" "file.snp" "file.ind"

g :: FormatSpec
g = VCFFormat "file.vcf"
```

This would declare `f` to be of type `FormatSpec`, and of value `PlinkFormat` with three filenames (Plink-formatted genotype data comes as three files). Similarly, `g` would be also be of type `FormatSpec`, but would have a `VCFFormat` value with one specified filename. So in Haskell we can easily define alternative branches of options at the type level. And the type system enforces these types at compile time. It would be a compiler error if we specify a value of type `FormatSpec` in any other shape as either of these two formats shown above. `PlinkFormat` and `VCFFormat` are called "Constructors" in Haskell. They act like a function that returns a type. Specifically, `PlinkFormat` is a function with three arguments (in Haskell, function arguments are just listed after the function name separated by whitespace), and `VCFFormat` a function with one argument. The equivalent in python would be 

```python
f = PlinkFormat("file.geno", "file.snp", "file.ind")
g = VCFFormat("file.vcf")
```

The final data type in the Options record is `IndividualsSpec`, which again has two alternatives. The first constructor `IndividualsByFile` takes a filename, and specifies that selected individuals should be given in a file (listed, say, with their IDs line by line). The second constructor `IndividualsByList` takes a list of strings, and specifies that individuals are given as a list of strings directly through the command line interface.

I hope you can appreciate that this data structure makes it as clear as it can possibly be i) what is needed in terms of program input, and ii) what the options are for various parameters. You will also appreciate that this data structure is already of considerable complexity. It has nested elements, alternatives, custom data types... at the same time I don't think it's unrealistic. I happen to have such interfaces in my programs, and I would argue that many bioinformatics tools have comparably complex interfaces, many even more complex.

So before we now dive into how to parse command line options into this structure, keep in mind that the type system is our friend: It will make sure that our parsing code will result in exactly the right shape for this data structure. Whatever our parsing code, the main program routine will simply take a value of type `Options` as input. So we can be fully relaxed when actually coding the main program logic later, because we know that all command-line input is passed - by construction - in the right shape, and our main program routine can simply query the main options value to process the different options.

## Building a parser for the Option type bottom-up

So clearly, parsing all these elements from the command line as arguments is challenging. For example, in python, using the popular and powerful package [argparse](https://docs.python.org/3/library/argparse.html), we surely can define an interface with command line options that allow for all the data needed to fill our nested data type `Options`, but there wouldn't be much help ensuring that the shape of Options is correct. For example, there wouldn't be any way (to my knowledge) to already ensure at the parsing level that the input file options either come with three files or with one, but not, say, with two or four.

In `argparse`, and many other command line libraries in python and elsewhere, you can only specify whether a given option is required or optional, and whether the arguments parsed should be accumulated in lists or not. But you can't easily automatically ensure that only one of each alternative, including specific additional arguments is allowed. So you'd end up coding with tons of if-statements making sure that only a specific combination of input choices is given, or you need to specify some logic of ignoring specific options if others are set. For example, you could parse both a filename and a list of strings for the `IndividualsSpec` type and then, say, ignore the list of strings once a filename is given. But that wouldn't make it crystal clear to the user that she has done a mistake in specifying both alternatives simultaneously. The only way to do that is to add manual validation logic using if statements, which is verbose, error prone and hard to maintain at larger scale.

Haskell has an absolutely beautiful library for this purpose, called [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative). It defines functions to design _composable_ command line option parsers, and it utilizes the power of Haskell's strong type system to help ensuring the correct shapes of the input data structure.

In `optparse-applicative` all the parsing logic for turning input from the command line into Haskell datatypes is encapsulated in a datatype called `Parser`. Specifically, in the end we need an object of type `Parser Options`, which then parses all input from the command line into our target datatype `Options` as defined above. So how do we build something of type `Parser Options`? The beauty of composable parsers is that you can create such a complex parser from much simpler and smaller components. To showcase this composability I'll follow a bottom-up approach here. So let's start with some basics. First, here is a command line parser for a value of type `Double`, for example for the missingness-threshold:

```haskell
missingnessParser :: Parser Double
missingnessParser = option auto (long "missingness" <> short 'm')
```

Here, `option auto` are functions from `optparse-applicative` and just declare a command line option with an automatic string-parser. The expression in the bracket using `long` and `short` gives specifics for the specified command line option. They define that this option can be called either in the long form, for example via `--missingness 0.99` or with the short form `-m 0.99`. The operator `<>` is just a concatenation operator for data types implementing the "Monoid" interface. You will see lots of inline operators in Haskell.

We can similarly easily define a Parser for a `Bool` type:
```haskell
verboseParser :: Parser Bool
verboseParser = switch (long "verbose" <> short 'v')
```
where we define this option to be parsed as a switch that can be triggered either using `--verbose` or via `-v`, without any arguments. If the switch is set, parse this option as `True`, if not parse as `False`.

Now it gets interesting. How do we parse a value of type `SummaryStatSpec`? As you see from above, `SummaryStatSpec` can take one of three possible constructions (`Heterozygosity`, `SegregatingSites` or `HardyWeinbergDeviation`). So we somehow need command line parsers that reflect these three alternatives. There are different ways to specify this, but here I would like the user to use flags `--heterozygosity` or `--segregatingSites` or `--hardyWeinbergDev`, and to parse these into the respective alternative values. To do this, we first define a parser for each value:

```haskell
HetParser :: Parser SummaryStatSpec
HetParser = flag' Heterozygosity (long "heterozygosity")

SegSitesParser :: Parser SummaryStatSpec
SegSitesParser = flag' SegregatingSites (long "segregatingSites")

HWParser :: Parser SummaryStatSpec
HWParser = flag' HardyWeinbergDeviation (long "hardyWeinbergDev")
```

where `flag'` is again a function defined in `Options.Applicative` which declares an option to be a simple flag returning a specific value when set. And here is now where the first composability magic starts: We can now compose these three parsers into one using the so-called applicative interface. Specifically, the operator `<|>` defined as "Alternative", joins multiple parsers into one, trying one after another and yielding an error either if none of the alternatives fits (because the user has mis-spelled an option or forgot them) or if more than one fits (because the user has given multiple options).

```haskell
summaryStatParser :: Parser SummaryStatSpec
summaryStatParser = HetParser <|> SegSitesParser <|> HWParser
```

This kind of composition of objects to build more complex types of objects from simpler ones is at the heart of Haskell, and we'll see it again further below. If you're worried about verbosity, note that we could have written this much more compact using `where`

```haskell
summaryStatParser :: Parser SummaryStatSpec
summaryStatParser = hetParser <|> segSitesParser <|> hwParser
  where
	hetParser      = flag' Heterozygosity (long "heterozygosity")
    segSitesParser = flag' SegregatingSites (long "segregatingSites")
	hwParser       = flag' HardyWeinbergDeviation (long "hardyWeinbergDev")
```

and let Haskell infer types of the three sub-parsers automatically, saving us some boilerplate type declarations.

Moving on, next on the list is parsing options into a `FormatSpec` type, which is more challenging, because we have two constructors (`PlinkFormat`, and `VCFFormat`) which in turn take additional arguments. We first deal with `PlinkFormat`. So, `PlinkFormat` is a constructor for type `FormatSpec`, which takes three filenames as arguments. It's easy to define parsers for the three files:

```haskell
genoParser = strOption (long "genoFile")
snpParser  = strOption (long "snpFile")
indParser  = strOption (long "indFile")
```

which are all of type `Parser FilePath` (omitting the type declaration for brevity). So, now we have a problem. Our constructor takes three arguments of type `FilePath` and returns a `FormatSpec`. However, what we actually need is some way of consuming arguments of type `Parser FilePath` and returning a `Parser FormatSpec`. So we need some way of pulling the constructor inside the `Parser` type. That's exactly what an "Applicative" type, like `Parser`, allows us to do. We have already made use of "Applicative" above using the operator `<|>`. We now see two further operators, `<$>` and `<*>`:

```haskell
plinkFormatParser :: Parser FormatSpec
plinkFormatParser = PlinkFormat <$> genoParser <*> snpParser <*> indParser
  where
    genoParser = strOption (long "genoFile")
	snpParser  = strOption (long "snpFile")
	indParser  = strOption (long "indFile")
```

Notice how the resulting type is actually of type `Parser FormatSpec`, which is exactly what we want here. It's a piece of cake to now add a parser of the same type but for the `VCFFormat` constructor:

```haskell
vcfFormatParser :: Parser FormatSpec
vcfFormatParser = VCFFormat <$> vcfFileParser
  where
    vcfFileParser = strOption (long "vcfFile")
```

Now, we already know how to construct these two parsers into a single `FormatSpec` parser, using alternative composition:
```haskell
formatParser :: Parser FormatSpec
formatParser = PlinkFormatParser <|> VCFFormatParser
```

Here, `FormatParser` is a single parser for the complex datatype `FormatSpec`, which includes all branching options and comes with guaranteed correctness of parsing all arguments for both possible format branches. We don't need to write evaluation code for this, the parser will ensure that the input is all type-correct. This specifically means that - without us having to implement anything special - this parser will automatically make sure that _if_ the user would like to go via Plink input, all three options `--genoFile`, `--snpFile` and `--indFile` must be set, and simultaneously `--vcfFile` _must not_ be set. Alternatively, if the user wants to specify input in VCF Format, they should use `--vcfFile`. So automatically, presence of either all three Plink file options or presence of only the single VCF file is guaranteed by the parser, or it will fail.

The final ingredient for our full `Options` parser is a parser for `IndividualsSpec`. Again we have two constructors to deal with. The first one is something we already know, `IndividualsByFile` simply takes a `FilePath` argument, similarly to `VCFFormat`. But the other constructor, `IndividualsByList` takes a list of string values. One way to construct a command line parser for a list type is to simply allow multiple inputs of a given option. Here, perhaps we would like to support something like `--ind Individual1 --ind Individual2 --ind Individual3` to be parsed into a list `["Individual1", "Individual2", "Individual3"]`. This is possible, again through a special kind of composition. Specifically, there is the combinator function `some` in the `Control.Applicative` library, which has the following type signature in this case:

```haskell
some :: Parser a -> Parser [a]
```

So it's a function that takes a Parser for an arbitrary type `a` and turns it into a parser for type `[a]`, which is a list type. Note that `some` requires the resulting list to have at least one element, whereas a very similar function, `many` would also parse zero input. We use `some` here, because we'd like the user to at least specify one input individual. So here is the complete parser for the `IndividualsSpec` Parser:

```haskell
individualsParser :: Parser IndividualsSpec
individualsParser = individualsFileParser <|> individualsListParser
  where
    individualsFileParser = IndividualsByFile <$>
			                strOption (long "individualsFile")
	individualsListParser = IndividualsByList <$>
		                    some (strOption (long "ind"))
```

## Putting it all together

Now for the final piece of magic, we will construct the full parser for the entire `Options` type. We have already seen Applicative syntax using `<$>` and `<*>` to create structured data types. But how do we use that for `Options`, which is a record type? It turns out that in Haskell you can use record type constructors also by providing all record fields as ordered arguments. So, say we wanted to construct an object called `opts` of type `Options`, instead of writing 

```haskell
opts :: Options
opts = Options {
	missingnessThreshold = 0.9,
	verbose = True,
	summaryStat = Heterozygosity,
	inputFormat = VCFFormat "input.vcf",
	individuals = IndividualsByList ["Ind1", "Ind2"]
}
```
which would be clear - if slightly verbose - record construction, we can also write 
```haskell
opts :: Options
opts = Options 0.9 True Heterozygosity (VCFFormat "input.vcf")
	   (IndividualsByList ["Ind1", "Ind2"])
```
where we simply have listed all the record entries as arguments to the `Options` constructor. This kind of ordered-argument syntax is not always clearer than using explicit record-construction, but here we need it to use Applicative syntax. Watch:

```haskell
optionsParser :: Parser Options
optionsParser = Options <$> missingnessParser <*> verboseParser <*>
	 			summaryStatParser <*> formatParser <*> individualsParser
```

Notice how this looks structurally almost exactly the same as the code block before. The only difference is that i) the argument types are not `Double`, `Bool`, ... but `Parser Double`, `Parser Bool`, ... ii) the return type isn't `Options` but `Parser Options`, and iii) the constructor and arguments are connected via `<$>` and `<*>`. I cannot stress enough how elegantly this composition is being made possible using very generic type-class interfaces like Applicative (which is delivered through a base library, not through the core language itself), and how automatic this makes all the complex parsing logic. By construction, the overall parser of type `Parser Options` is guaranteed to return a valid `Options` type with all its internal branches parsed and set correctly. And we've built this by composing together simpler to intermediate to the final parser. Lastly, we didn't even have to understand much about the `Parser` datatype, other than that it supports Applicative syntax.

Of course, this parser comes with an integrated help message, and there are tons of extra options to tweak how that looks and how to invoke help, and so on, which I haven't touched here. If you're interested to see this complete example, check out [this github repository](https://github.com/stschiff/haskell-cli-example) which contains the full example. The help output, automatically generated from `optparse-applicative` looks like this:

```haskell
Usage: haskell-cli-example-exe [-m|--missingness NUMBER] [-v|--verbose] 
                               (--heterozygosity | --segregatingSites | 
                                 --hardyWeinbergDev) 
                               (--genoFile FILE --snpFile FILE --indFile FILE | 
                                 --vcfFile FILE) 
                               (--individualsFile FILE | --ind NAME)
  Hello, this is a toy example for how to design command line interfaces in
  Haskell

Available options:
  -m,--missingness NUMBER  A missingness threshold (default: 0.5)
  -v,--verbose             verbose output
  --heterozygosity         compute the rate of heterozygosity for each
                           individual
  --segregatingSites       compute the rate of segregating sites for each
                           individual
  --hardyWeinbergDev       compute the average deviation from Hardy-Weinberg
                           equilibrium for each individual
  --genoFile FILE          the input genotype file
  --snpFile FILE           the input snp file
  --indFile FILE           the input individual file
  --vcfFile FILE           the input VCF file
  --individualsFile FILE   list individuals in the file given
  --ind NAME               list individuals directly on the command line. Option
                           can be given multiple times, once for each individual
  -h,--help                Show this help text
```

Of course, I've put in more help text into the parsers, but I'd like to emphasize that the layout of the command line usage on top contains all the conditioning that we've designed. For example, the bit `(--genoFile FILE --snpFile FILE --indFile FILE | --vcfFile FILE)` makes it clear to the user that _either_ the first triple of options _or_ the last option `--vcfFile` can be given. Those are strict alternatives, automatically generated by the Compiler, and the parser will fail if the user violates this.

## Conclusion

I know this was likely a bit more of Haskell than what I promised at the beginning, sorry. But I hope you got a glimpse into its power, expressiveness and composability. This ultimately allows programming by specification, which means that the programmer can focus more on specifying _what_ should happen rather than _how_ it happens. We started out by _specifying_ the interface through a data structure that already contained much of the logic we need for parsing options. This - together with a powerful compiler that guarantees type-correctness throughout -  leaves very little room for bugs and ultimately makes the program safer and its interface clearer.

