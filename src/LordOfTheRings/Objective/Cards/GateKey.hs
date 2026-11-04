module LordOfTheRings.Objective.Cards.GateKey (gateKey, gateKeyUpgraded) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Campaign (CampaignLogKey (Secrets))
import LordOfTheRings.Card (Card (EncounterCard, PlayerCard), genEncounterCard, genPlayerCard)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Campaign (getTempleSecrets)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))

-- | Guarded by Temple Guardian, attached to it as the scenario begins: discarded as the
-- Guardian is defeated, it returns to the staging area, as any objective does.
newtype GateKey = GateKey ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gateKey :: ObjectiveCard GateKey
gateKey = objective GateKey Cards.gateKey

-- | Its Upgraded side (the Dream-chaser campaign): "Action: Exhaust Gate Key and attached
-- hero to look at the facedown side of a Temple of the Deceived location or secret."
gateKeyUpgraded :: ObjectiveCard GateKey
gateKeyUpgraded = objective GateKey Cards.gateKeyUpgraded

upgraded :: ObjectiveAttrs -> Bool
upgraded a = toCardCode a == toCardCode Cards.gateKeyUpgraded

-- | If unattached, return Gate Key to the staging area and it gains: "Action: Exhaust a
-- hero to claim Gate Key and attach it to that hero."
-- Action: Exhaust Gate Key and attached hero and spend 3 resources from attached hero's
-- resource pool to look at the facedown side of a Temple of the Deceived.
instance HasAbilities GateKey where
  getAbilities (GateKey a) =
    [ anyPlayer $ restricted a 1 (exists (HeroControlledBy ActivePlayer <> ReadyHero)) $ ActionAbility Free
    | a.placement == StagingArea
    ]
      <> [ anyPlayer
            $ restricted
              a
              2
              ( exists (HeroWithId hid <> HeroControlledBy ActivePlayer <> ReadyHero <> HeroWithResourcesAtLeast (if upgraded a then 0 else 3))
                  <> AnyCriteria (exists (LocationWithCardCode $ toCardCode Locations.templeOfTheDeceived) : [CampaignRecorded Secrets | upgraded a])
              )
            $ ActionAbility Free
         | AttachedTo (HeroTarget hid) <- [a.placement]
         , not a.objectiveExhausted
         ]

instance RunMessage GateKey where
  runMessage msg o@(GateKey attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
      chooseOneM pid do
        questionLabeled "gateKey.claim"
        causedBy attrs
        targets heroes \h -> do
          exhaust h
          attachObjective attrs.id h
      pure o
    UseCardAbility pid (isSource attrs -> True) 2 _ -> do
      for_ [h | AttachedTo h@(HeroTarget _) <- [attrs.placement]] \h -> do
        exhaust attrs
        exhaust h
        unless (upgraded attrs) $ removeTokens attrs h Resource 3
        temples <- select (LocationWithCardCode $ toCardCode Locations.templeOfTheDeceived)
        secrets <- if upgraded attrs then getTempleSecrets else pure []
        under <- concat <$> for secrets \(pos, code) -> map (,code) <$> select (LocationAtPosition pos)
        -- a secret is picked by the location it lies under
        chooseOneM pid do
          questionLabeled "gateKey.look"
          causedBy attrs
          targets (ordNub $ temples <> map fst under) \l -> chooseOrRunOneM pid do
            questionLabeled "gateKey.lookAt"
            causedBy attrs
            when (l `elem` temples) $ labeled "gateKey.facedownSide" $ lookAtFacedownSide pid l
            for_ (lookup l under) \code -> labeled "gateKey.secret" do
              secret <-
                if code == toCardCode Allies.starla
                  then PlayerCard <$> genPlayerCard Allies.starla (Just pid)
                  else EncounterCard <$> genEncounterCard Enemies.recurringNightmare
              chooseOneM pid do
                questionLabeled "gateKey.secretIs"
                causedBy attrs
                showing [secret]
                labeled "continue" $ pure ()
      pure o
    _ -> GateKey <$> runMessage msg attrs
