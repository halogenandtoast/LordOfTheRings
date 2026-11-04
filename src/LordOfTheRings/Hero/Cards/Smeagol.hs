module LordOfTheRings.Hero.Cards.Smeagol (smeagol) where

import LordOfTheRings.Card (genEncounterCard)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

-- | Sméagol, hero side faceup: flipped, he is the enemy of
-- "LordOfTheRings.Enemy.Cards.GollumASitE". He cannot be a starting hero in the Shadows
-- of Mirkwood cycle (see 'LordOfTheRings.Decklist.barredHeroes').
newtype Smeagol = Smeagol HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

smeagol :: HeroCard Smeagol
smeagol = hero Smeagol Cards.smeagol

-- | Cannot have attachments.
instance HasModifiersFor Smeagol where
  getModifiersFor (Smeagol a) = modifySelf a [CannotHaveAttachments]

-- | Response: After you travel to a location, raise your threat by 1 to draw 1 card.
-- The players travel together, so each of them travels.
instance HasAbilities Smeagol where
  getAbilities (Smeagol a) = [mkAbility a 1 $ ResponseAbility (AnyTraveledWindow After) (RaiseThreatCost 1)]

instance RunMessage Smeagol where
  runMessage msg s@(Smeagol a) = case msg of
    -- Setup: Shuffle 2 copies of Stinker into the encounter deck.
    -- The copies come from outside his owner's deck. A hero's Setup resolves at step 7
    -- with the others, after the scenario's own: Caleb's ruling, as reported on the FFG
    -- forum ("Timing of Setup"), overruling an earlier one.
    DoStep 7 Setup -> do
      stinkers <- replicateM 2 (genEncounterCard Treacheries.stinker)
      traverse_ setAside stinkers
      shuffleSetAsideIntoEncounterDeck stinkers
      Smeagol <$> runMessage msg a
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> Smeagol <$> runMessage msg a
