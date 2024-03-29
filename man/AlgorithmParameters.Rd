% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algorithm_parameters.R
\name{AlgorithmParameters}
\alias{AlgorithmParameters}
\title{R6 class wrapping parameter classes together}
\description{
Wraps roost parameters, parameters for each resistance layer, resolution, etc.
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-AlgorithmParameters-new}{\code{AlgorithmParameters$new()}}
\item \href{#method-AlgorithmParameters-clone}{\code{AlgorithmParameters$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AlgorithmParameters-new"></a>}}
\if{latex}{\out{\hypertarget{method-AlgorithmParameters-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AlgorithmParameters$new(
  roost,
  roadResistance = NULL,
  riverResistance = NULL,
  landscapeResistance = NULL,
  linearResistance = NULL,
  lampResistance = NULL,
  resolution = 1,
  n_circles = 1
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AlgorithmParameters-clone"></a>}}
\if{latex}{\out{\hypertarget{method-AlgorithmParameters-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AlgorithmParameters$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
