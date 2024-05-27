Générateur d'Applications COBOL
Ce projet est un générateur d'applications COBOL qui facilite la création de fichiers et d'applications en fonction des entrées de l'opérateur. L'outil est conçu pour augmenter la productivité en automatisant la génération de code nécessaire pour les opérations courantes en environnement COBOL.

Prérequis
Avant de commencer, assurez-vous d'avoir installé les outils suivants :

COBOL Compiler (par exemple, GnuCOBOL)
Git (pour le clonage du dépôt)
Installation
Clonez le dépôt sur votre machine locale en utilisant la commande suivante :

bash
Copier le code
git clone [URL_DU_REPO]
cd [NOM_DU_REPO]
Utilisation
Pour utiliser l'application, suivez ces étapes simples :

bash
Copier le code
# Naviguez dans le dossier du projet
cd chemin/vers/le/projet

# Exécutez le script principal
cobc -x -free votre_programme_principal.cob
Fonctionnalités
Le générateur propose les fonctionnalités suivantes, réparties en plusieurs modules :

Partie PS
Lire et écrire des fichiers séquentiels.
Trier et fusionner des fichiers.
Partie SGBD
Accéder à des bases de données.
Générer des requêtes SQL de base, comme SELECT COUNT(*) et UPDATE.
Partie Sous-routines
Création et intégration de sous-routines COBOL.
Partie WS
Gestion des Copybooks.
Partie Procédure
Génération de code testable avec des paragraphes type.
Pour aller plus loin
Améliorations futures pour inclure une interface graphique et plus d'options de nomenclature.
Contribution
Les contributions sont les bienvenues. Si vous souhaitez contribuer, veuillez forker le dépôt, créer une branche, apporter vos modifications, et soumettre une pull request.

Auteurs
safaa krim 
