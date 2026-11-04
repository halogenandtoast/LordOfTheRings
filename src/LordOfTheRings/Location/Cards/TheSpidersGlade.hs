module LordOfTheRings.Location.Cards.TheSpidersGlade (theSpidersGlade) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Trait (Trait (Spider))

newtype TheSpidersGlade = TheSpidersGlade LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theSpidersGlade :: LocationCard TheSpidersGlade
theSpidersGlade = location TheSpidersGlade Cards.theSpidersGlade

bilbo :: HeroMatcher
bilbo = HeroWithTitle "Bilbo Baggins"

-- | Immune to player card effects.
-- Travel: Exhaust Bilbo Baggins to travel here.
instance HasModifiersFor TheSpidersGlade where
  getModifiersFor (TheSpidersGlade a) = do
    canExhaust <- selectAny (bilbo <> ReadyHero)
    modifySelf a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | not canExhaust]

-- | Forced: At the start of each quest phase, search the encounter deck for 1 Spider enemy
-- and add it to the staging area. (2 Spider enemies instead if 3 or more players are at
-- this stage.) Shuffle the encounter deck.
instance HasAbilities TheSpidersGlade where
  getAbilities (TheSpidersGlade a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

instance RunMessage TheSpidersGlade where
  runMessage msg l@(TheSpidersGlade a) = case msg of
    TravelTo lid | lid == a.id -> do
      heroes <- select (bilbo <> ReadyHero)
      for_ (take 1 heroes) exhaust
      TheSpidersGlade <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      -- the players at this stage are those in its area, while the first player is apart
      players <- length <$> getRemainingPlayers
      lead <- getLead
      replicateM_ (if players >= 3 then 2 else 1)
        $ searchEncounterDeckToStaging lead (CardWithTrait Spider <> CardWithType EnemyType)
      pure l
    _ -> TheSpidersGlade <$> runMessage msg a
