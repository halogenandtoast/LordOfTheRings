module LordOfTheRings.Location.Cards.TheGreatBridge (theGreatBridge) where

import LordOfTheRings.Card (cdThreatCost, toCardCode, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.JourneyInTheDark.Helpers (getBalrog)

newtype TheGreatBridge = TheGreatBridge LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theGreatBridge :: LocationCard TheGreatBridge
theGreatBridge = location TheGreatBridge Cards.theGreatBridge

-- | Immune to player card effects.
instance HasModifiersFor TheGreatBridge where
  getModifiersFor (TheGreatBridge a) = modifySelf a [ImmuneToPlayerCardEffects]

{- | Response: When The Great Bridge is explored, discard a hero from play to deal X damage
to The Balrog. X is that hero's threat cost. Then, The Balrog loses all keywords for the
remainder of the game. Any player may trigger this response.

It is offered while The Balrog is in play: otherwise it could not change the game. The
player who triggers it discards a hero he controls. The Balrog's only keyword is
Indestructible (FAQ, "How many keywords does The Balrog (RD 44) have?").
-}
instance HasAbilities TheGreatBridge where
  getAbilities (TheGreatBridge a) =
    [anyPlayer $ restricted a 1 (exists $ EnemyWithCardCode $ toCardCode Enemies.theBalrog) $ response $ ExploredWindow When a.id]

instance RunMessage TheGreatBridge where
  runMessage msg l@(TheGreatBridge a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      balrog <- getBalrog
      chooseOneM pid do
        questionLabeled "theGreatBridge.discardHero"
        causedBy a
        targets heroes \h -> do
          hero <- getHero h
          discard h
          for_ balrog \b -> do
            dealDamage a b (fromMaybe 0 $ cdThreatCost $ toCardDef hero)
            createEffect a b [LoseKeyword Indestructible] UntilSpent
      pure l
    _ -> TheGreatBridge <$> runMessage msg a
