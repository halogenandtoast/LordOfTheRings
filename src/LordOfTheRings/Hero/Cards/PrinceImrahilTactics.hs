module LordOfTheRings.Hero.Cards.PrinceImrahilTactics (princeImrahilTactics) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype PrinceImrahilTactics = PrinceImrahilTactics HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

princeImrahilTactics :: HeroCard PrinceImrahilTactics
princeImrahilTactics = hero PrinceImrahilTactics Cards.princeImrahilTactics

-- | Combat Action: Spend 1 resource from Prince Imrahil's resource pool to search the top 5
-- cards of your deck for an ally who shares at least 1 Trait with him, and put that ally
-- into play. Shuffle your deck. At the end of the phase, if that ally is still in play,
-- shuffle it into your deck. (Limit once per round.)
instance HasAbilities PrinceImrahilTactics where
  getAbilities (PrinceImrahilTactics a) =
    [ limited (GroupLimit PerRound 1)
        $ restricted a 1 (DuringPhase CombatPhase)
        $ ActionAbility (ResourceCost (toTarget a) 1)
    ]

instance RunMessage PrinceImrahilTactics where
  runMessage msg h@(PrinceImrahilTactics attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      let sharing = CardWithType AllyType <> CardSharesTraitWithHero (HeroWithId attrs.id)
      searchDeckIntoPlay pid attrs 5 sharing 1 [ShuffleIntoDeckAtEndOfPhase]
      pure h
    _ -> PrinceImrahilTactics <$> runMessage msg attrs
