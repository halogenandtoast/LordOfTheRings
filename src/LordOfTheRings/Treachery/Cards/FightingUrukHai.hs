module LordOfTheRings.Treachery.Cards.FightingUrukHai (fightingUrukHai) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Trait (Trait (UrukHai))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FightingUrukHai = FightingUrukHai TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril, as its card def says.
fightingUrukHai :: TreacheryCard FightingUrukHai
fightingUrukHai = treachery FightingUrukHai Cards.fightingUrukHai

{- | When Revealed: Search the encounter deck and discard pile for an Uruk-hai enemy, reveal
it, and put it into play engaged with you. Shuffle the encounter deck. Until the end of the
round, each enemy engaged with you adds its threat to the total threat in the staging area.
-}
instance RunMessage FightingUrukHai where
  runMessage msg t@(FightingUrukHai attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      searchEncounterDeckAndDiscardToRevealEngaged you attrs (CardWithType EnemyType <> CardWithTrait UrukHai)
      roundModifier attrs GameTarget (EngagedEnemiesAddThreat $ EnemyEngagedWith $ PlayerWithId you)
      pure t
    _ -> FightingUrukHai <$> runMessage msg attrs
