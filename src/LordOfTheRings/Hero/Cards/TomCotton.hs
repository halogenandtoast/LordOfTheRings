module LordOfTheRings.Hero.Cards.TomCotton (tomCotton) where

import LordOfTheRings.Card (CardType (AllyType), cdCardTraits, toCardDef)
import LordOfTheRings.Helpers.Modifiers (cardsPlayedThisRound)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype TomCotton = TomCotton HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

tomCotton :: HeroCard TomCotton
tomCotton = hero TomCotton Cards.tomCotton

-- | While you are engaged with an enemy with an engagement cost higher than your threat,
-- the first Hobbit ally you play each round does not require a resource match. The first
-- one played, while engaged or not: a later one is not the first.
instance HasModifiersFor TomCotton where
  getModifiersFor (TomCotton a) = do
    let you = a.owner
    engaged <- selectAny (EnemyEngagedWith (PlayerWithId you) <> EnemyWithEngagementCostAboveThreatOf you)
    played <- cardsPlayedThisRound
    let hobbitPlayed = any (\(p, code, t) -> p == you && t == AllyType && Hobbit `member` cdCardTraits (toCardDef code)) played
    when (engaged && not hobbitPlayed)
      $ modified a (PlayerTarget you) [NextIgnoresResourceMatch (CardWithType AllyType <> CardWithTrait Hobbit)]

-- | Response: After a Hobbit ally enters play, it gets +2 attack until the end of the
-- round.
instance HasAbilities TomCotton where
  getAbilities (TomCotton a) = [mkAbility a 1 $ response $ AllyEnteredPlayWindow After Nothing (Just Hobbit)]

instance RunMessage TomCotton where
  runMessage msg h@(TomCotton attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ (take 1 [t | Window _ (AllyEnteredPlay t _ _) <- windows]) \t -> roundModifier attrs t (AttackModifier 2)
      pure h
    _ -> TomCotton <$> runMessage msg attrs
