module LordOfTheRings.Location.Cards.DungeonCell (dungeonCell) where

import LordOfTheRings.Card (Card (PlayerCard), HasCardDef, cdWillpower, toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheDungeonsOfCirithGurat.Helpers (guardingAnObjective)

newtype DungeonCell = DungeonCell LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge, as its card def says.
dungeonCell :: LocationCard DungeonCell
dungeonCell = location DungeonCell Cards.dungeonCell

-- | Dungeon Cell gets +X threat where X is the printed willpower of the attached objective.
instance HasModifiersFor DungeonCell where
  getModifiersFor (DungeonCell a) = do
    captives <- charactersGuardedBy a.cardId
    willpower <- sum <$> for captives \case
      HeroTarget hid -> printed <$> getHero hid
      AllyTarget aid -> printed <$> getAlly aid
      _ -> pure 0
    when (willpower > 0) $ modifySelf a [ThreatModifier willpower]
   where
    printed :: HasCardDef c => c -> Int
    printed = fromMaybe 0 . cdWillpower . toCardDef

-- | While Dungeon Cell is in the staging area, it gains: "Forced: After an ally is
-- destroyed, attach it to Dungeon Cell if there is no objective attached to Dungeon Cell."
instance HasAbilities DungeonCell where
  getAbilities (DungeonCell a) = [mkAbility a 1 $ forced $ AllyDestroyedWindow After | a.placement == StagingArea]

instance RunMessage DungeonCell where
  runMessage msg l@(DungeonCell a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      unlessM (guardingAnObjective a)
        $ for_ (take 1 [c | Window _ (CharacterDestroyed (AllyTarget _) c@(PlayerCard _)) <- ws]) \c -> guardCardWith c a
      pure l
    _ -> DungeonCell <$> runMessage msg a
