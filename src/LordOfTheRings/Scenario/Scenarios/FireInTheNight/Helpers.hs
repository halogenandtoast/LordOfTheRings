-- | What the cards of Fire in the Night share: Dagnir, Hrogar's Hill and the side quest
-- deck, which the scenario keeps in its meta.
module LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (
  getDagnir,
  getHrogarsHill,
  sideQuestX,
  damageHrogarsHill,
  dagnirAttacksFirstPlayer,
  dagnirAttacksHrogarsHill,
  revealSideQuest,
  sideQuestDeckOf,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (EncounterCard, toCardCode)
import LordOfTheRings.Classes.Entity (toId)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message (Message (ScenarioSpecific))
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (CannotDefendAgainst, ExcessDamageTo, UndefendedDamageTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Source
import LordOfTheRings.Target

getDagnir :: HasGame m => m (Maybe EnemyId)
getDagnir = headMay <$> select (EnemyWithCardCode $ toCardCode Enemies.dagnir)

getHrogarsHill :: HasGame m => m (Maybe QuestId)
getHrogarsHill = getsGame (fmap toId . find ((== toCardCode Quests.hrogarsHill) . toCardCode) . toList . entitiesQuests . gameEntities)

-- | "X is 1 more than the number of side quests in the victory display."
sideQuestX :: HasGame m => m Int
sideQuestX = (+ 1) <$> getSideQuestsInVictoryDisplay

-- | Deal damage to Hrogar's Hill, as "deal 3 damage to Hrogar's Hill" reads.
damageHrogarsHill :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
damageHrogarsHill source n = getHrogarsHill >>= traverse_ \hill -> when (n > 0) $ dealDamage source (QuestTarget hill) n

-- | "Dagnir makes an immediate attack against the first player."
dagnirAttacksFirstPlayer :: ReverseQueue m => m ()
dagnirAttacksFirstPlayer = getDagnir >>= traverse_ \d -> do
  lead <- getLead
  immediateAttack lead d

{- | "Dagnir makes an attack against Hrogar's Hill. Do not deal a shadow card for this
attack. The first player may declare a defender for this attack. Excess damage must be
dealt to Hrogar's Hill." As Trouble in Tharbad's attacks on Nalir, it is made against the
first player: undefended, its damage goes to Hrogar's Hill, and no other player's
character, a sentinel among them, defends it, for it is not made against a player.
-}
dagnirAttacksHrogarsHill :: (ReverseQueue m, Sourceable source) => source -> m ()
dagnirAttacksHrogarsHill source = do
  dagnir <- getDagnir
  hill <- getHrogarsHill
  for_ ((,) <$> dagnir <*> hill) \(d, h) -> do
    lead <- getLead
    others <- filter (/= lead) <$> getRemainingPlayers
    for_ others \p -> do
      cs <- charactersControlledBy p
      for_ cs \c -> attackModifier source c (CannotDefendAgainst $ EnemyWithId d)
    attackModifier source d (UndefendedDamageTo $ QuestTarget h)
    attackModifier source d (ExcessDamageTo $ QuestTarget h)
    immediateAttackWithoutShadow lead d

-- | The scenario reveals the top card of the side quest deck, adding it to the staging area.
revealSideQuest :: ReverseQueue m => m ()
revealSideQuest = push $ ScenarioSpecific "fireInTheNight.revealSideQuest" Null

-- | The side quest deck the scenario's meta holds, top first.
sideQuestDeckOf :: Value -> [EncounterCard]
sideQuestDeckOf = fromMaybe [] . parseMaybe (withObject "meta" (.: "sideQuestDeck"))
