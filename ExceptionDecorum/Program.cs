using System;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.MSBuild;
using TypeInfo = System.Reflection.TypeInfo;

namespace ExceptionDecorum
{
	class Program
	{
		static Dictionary<MethodDeclarationSyntax, HashSet<ITypeSymbol>> potentiallyMissingDeclarationLocations = new Dictionary<MethodDeclarationSyntax, HashSet<ITypeSymbol>>();
		static void Main(string[] args)
		{
			string solutionPath = @"c:\www\tombola.web\src\Tombola.sln";
			//string solutionPath = @"C:\Users\keith.barrow\Desktop\Bar\HelloWorld\HelloWorld.sln";
			var workspace = MSBuildWorkspace.Create();
			var solution = workspace.OpenSolutionAsync(solutionPath).Result;

			foreach (var project in solution.Projects)
				ProcessProject(project);
			//Could theoretically fix here!
			OutputFindingsToConsole();
			Console.ReadKey();
		}

		private static void OutputFindingsToConsole()
		{
			foreach (var potentiallyMissingDeclaration in potentiallyMissingDeclarationLocations)
			{
				
				Console.WriteLine("File Path:\t{0}", potentiallyMissingDeclaration.Key.GetLocation());
				Console.WriteLine("MethodName:\t{0}", potentiallyMissingDeclaration.Key.Identifier.ValueText);
				Console.WriteLine("Starting At:\t{0}", potentiallyMissingDeclaration.Key.GetLocation().SourceSpan.Start);
				Console.WriteLine("Missing Exeption Declarations:");
				Console.ForegroundColor = ConsoleColor.DarkYellow;
				foreach (var exceptionType in potentiallyMissingDeclaration.Value)
				{
					Console.WriteLine("\t\t" + exceptionType.Name);
				}
				Console.ResetColor();
			}
		}


		static void ProcessProject(Project project)
		{
			//TODO: ignore unit test projects....
			var compilation = project.GetCompilationAsync().Result;
			if (compilation.Language != "C#")
				return;
			foreach (var document in project.Documents)
				ProcessDocument(document);
		}

		static void ProcessDocument(Document document)
		{
			SyntaxTree syntaxTree;
			if (!document.TryGetSyntaxTree(out syntaxTree))
				return;

			SyntaxNode rootNode = syntaxTree.GetRoot();
			var throwClauses = rootNode.DescendantNodesAndSelf().OfType<ThrowStatementSyntax>();
			var semanticModel = document.GetSemanticModelAsync().Result;
			foreach (var throwClause in throwClauses)
			{
				ProcessThrowClause(semanticModel, throwClause);
			}
		}

		static void ProcessThrowClause(SemanticModel semanticModel, ThrowStatementSyntax throwClause)
		{
			var thrownTypeExtractor = new ThrownTypeExtractor();
			ITypeSymbol typeSymbol = thrownTypeExtractor.GetThrownExceptionTypeSymbol(semanticModel, throwClause);
			if (typeSymbol == null)
				return;
			if (!typeSymbol.ContainingNamespace.ToDisplayParts().Any(x => x.Kind == SymbolDisplayPartKind.NamespaceName
																		  && x.Symbol.Name.ToLowerInvariant() == "tombola"))
				return;

			var containingMethods = throwClause.AncestorsAndSelf().OfType<MethodDeclarationSyntax>();
			if (containingMethods == null || containingMethods.Count() == 0)
				return;
			var containingMethod = containingMethods.Last();
			if (containingMethod == null)
				return;
			if (!DoesContainingMethodDeclareExceptionType(containingMethod, typeSymbol.Name))
				AddToMissingDefinitions(containingMethod, typeSymbol);
		}

		static void AddToMissingDefinitions(MethodDeclarationSyntax containingMethod, ITypeSymbol typeSymbol)
		{
			if (!potentiallyMissingDeclarationLocations.ContainsKey(containingMethod))
				potentiallyMissingDeclarationLocations.Add(containingMethod, new HashSet<ITypeSymbol>());
			potentiallyMissingDeclarationLocations[containingMethod].Add(typeSymbol);
		}

		static bool DoesContainingMethodDeclareExceptionType(MethodDeclarationSyntax containingMethod, string expectedExceptionTypeToDeclare)
		{
			var documentationTrivias = containingMethod.GetLeadingTrivia().Where(x => x.CSharpKind() == SyntaxKind.SingleLineDocumentationCommentTrivia);
			foreach (var documentationTrivia in documentationTrivias)
			{
				var documentationTriviaStructure = (DocumentationCommentTriviaSyntax)documentationTrivia.GetStructure();
				var xmlElements = documentationTriviaStructure.Content.OfType<XmlElementSyntax>();
				var exceptionDeclarations = xmlElements.Where(x => x.StartTag.Name.ToFullString().ToLowerInvariant() == "exception");

				return exceptionDeclarations.Any(x => x.StartTag.Attributes.Any(y => y.CSharpKind() == SyntaxKind.XmlCrefAttribute
																				&& ((XmlCrefAttributeSyntax)y).Cref.ToString() == expectedExceptionTypeToDeclare));
			}
			return false;
		}
	}
}
