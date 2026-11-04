module LordOfTheRings.Location.Cards.TrollCamp (trollCamp) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (trollEnemy)
import LordOfTheRings.Sphere (Sphere (Baggins))
import LordOfTheRings.Trait (Trait (Sack))

newtype TrollCamp = TrollCamp LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

trollCamp :: LocationCard TrollCamp
trollCamp = location TrollCamp Cards.trollCamp

-- | While Troll Camp is in the staging area, Troll enemies get +1 threat for each player in
-- the game.
instance HasModifiersFor TrollCamp where
  getModifiersFor (TrollCamp a) = when (a.placement == StagingArea) do
    players <- length <$> getRemainingPlayers
    trolls <- select trollEnemy
    for_ trolls \t -> modified a t [ThreatModifier players]

{- | While Troll Camp is in play, Bilbo Baggins gains: "Action: Exhaust Bilbo Baggins and
spend 1 Baggins resource to remove 1 Sack card from a character. Bilbo Baggins may trigger
this effect even with a Sack card attached to him."

The action is Troll Camp's to give, so a Sack card on Bilbo does not stop it; his
controller triggers it.
-}
instance HasAbilities TrollCamp where
  getAbilities (TrollCamp a) =
    [ anyPlayer
        $ restricted a 1 (exists (bilbo <> ReadyHero) <> CharactersAtLeast 1 (HeroWithAttached Sack) (AllyWithAttached Sack))
        $ ActionAbility (Costs [ExhaustHeroCost bilbo, SpendResourcesCost Baggins 1])
    ]
   where
    bilbo = HeroWithTitle "Bilbo Baggins" <> HeroControlledBy ActivePlayer

instance RunMessage TrollCamp where
  runMessage msg l@(TrollCamp a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      sacks <- filter onCharacter <$> objectivesWithTrait Sack
      chooseOrRunOneM pid do
        questionLabeled "trollCamp.removeSack"
        causedBy a
        targets sacks (returnToSackDeck . toId)
      pure l
    _ -> TrollCamp <$> runMessage msg a
   where
    onCharacter o = case o.placement of
      AttachedTo (HeroTarget _) -> True
      AttachedTo (AllyTarget _) -> True
      _ -> False
