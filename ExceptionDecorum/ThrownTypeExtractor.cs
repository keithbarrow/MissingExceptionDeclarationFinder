using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ExceptionDecorum
{
	public class ThrownTypeExtractor
	{
		SemanticModel SemanticModel { get; set; }
		ThrowStatementSyntax ThrowStatementSyntax { get; set; }

		public ITypeSymbol GetThrownExceptionTypeSymbol(SemanticModel semanticModel, ThrowStatementSyntax throwStatementSyntax)
		{
			SemanticModel = semanticModel;
			ThrowStatementSyntax = throwStatementSyntax;

			SyntaxNode syntaxNodeToGetTypeOf = ThrowStatementSyntax.Expression;

			if (syntaxNodeToGetTypeOf == null)
				syntaxNodeToGetTypeOf = GetCatchClaue();

			if (syntaxNodeToGetTypeOf == null)
				return null; //Not sure this is possible....
			
			return semanticModel.GetTypeInfo(syntaxNodeToGetTypeOf).Type;
		}

		private CatchClauseSyntax GetCatchClaue()
		{
			return ThrowStatementSyntax
						.Ancestors()
						.OfType<CatchClauseSyntax>()
						.LastOrDefault();
			
		}
	}
}
